#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <string>

#define PASS_NAME "CLogWave"
#define log_fatal errs() << PASS_NAME << ": FATAL: "

using namespace llvm;

namespace {

StringRef getTypeFromDbgInst(const DbgDeclareInst *dbg_inst) {
  /* TODO: how to handle arrays? */
  StringRef int_string("int");

  Metadata *raw = dbg_inst->getRawVariable();
  if (isa<DILocalVariable>(raw)) {
    DILocalVariable *local_var = dyn_cast<DILocalVariable>(raw);
    DIType *local_type = local_var->getType();
    if (local_type != NULL && local_type->getName().equals(int_string)) {
        return StringRef("reg");
    }
  }
  return StringRef("real");
}

StringRef getNameFromDbgInst(const DbgDeclareInst *dbg_inst) {
  Metadata *raw = dbg_inst->getRawVariable();
  if (isa<DILocalVariable>(raw)) {
    DILocalVariable *local_var = dyn_cast<DILocalVariable>(raw);
    return local_var->getName();
  }
  errs() << "Could Not Find Name in DbgInfo, perhaps compile the program with "
            "-g?\n";
  /* TODO: return better */
  return "";
}

struct VarContainer {
  std::string name;
  std::string type;
  std::string width;
};

class FuncContainer {
  /* All variables used by the Function */
  std::vector<VarContainer> vars;
  /* Name of the function */
  StringRef name;

public:
  FuncContainer(const llvm::Function &func) {
    name = func.getName();
    /* TODO: get all local variables, and push them to var */
    for (auto &BB : func) {
      for (auto Inst = BB.begin(); Inst != BB.end(); ++Inst) {
        if (isa<DbgDeclareInst>(Inst)) {
          /* This is a CallInst, isa returns true because
           * DbgDeclareInst inherits from CallInst
           * */
          const DbgDeclareInst *dbg_inst = dyn_cast<DbgDeclareInst>(Inst);
          StringRef ret_name = getNameFromDbgInst(dbg_inst);
          VarContainer vv;
          vv.name = std::string(func.getName().str() + std::string(".") +
                                ret_name.str());
          vv.type = getTypeFromDbgInst(dbg_inst);
          vv.width =
              std::string("64"); // TODO: write a func to get this from DbgInfo
          vars.push_back(vv);
        }
      }
    }
    /* TODO: get all global variables and push them to var */
  }
  StringRef getName() { return name; }

  std::vector<VarContainer> getVars() const { return vars; }

  void print() const {
    outs() << "Function: " << name << '\n';
    for (auto &i : vars) {
      outs() << '\t' << i.name << '\n';
    }
  }
};

/* TODO: Iterate over this to generate $scope ... $end statements */
class ScopeHierarchy {
  std::map<StringRef, FuncContainer *> scope_hierarchy;

public:
  void insert(const Function &func) {
    FuncContainer *fc = new FuncContainer(func);
    scope_hierarchy.insert({func.getName(), fc});
  }
  void generate_scope() const {
    for (auto &i : scope_hierarchy) {
      outs() << "$scope module " << i.first << " $end\n";
      for (auto &var : i.second->getVars()) {
        outs() << "$var " << var.type << ' ' << var.width << ' ' << var.name
               << ' ' << var.name << " $end\n";
      }
      outs() << "$upscope $end\n";
    }
  }
  void print() {
    for (auto i : scope_hierarchy) {
      i.second->print();
    }
  }
  ~ScopeHierarchy() {
    for (auto i : scope_hierarchy) {
      delete i.second;
    }
  }
};

class Node {
  std::vector<FuncContainer> funcs;
};

struct CLogWave : public llvm::PassInfoMixin<CLogWave> {
  const StringRef file_ptr_var_name = "MemTraceFilePtr";
  const StringRef vcd_dump_filename = "clogwave_dump.vcd";
  const StringRef trace_func_name = "my_trace";
  int store_cnt = 0;
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &);
  void addFilePtrDeclaration(llvm::Module &M);
  void addCallToFopen(Module &M, Function *fmain);
  void insertCallToFprintf(IRBuilder<> &Builder, llvm::Function &func,
                           llvm::Module &M);
  void addTraceToInst(Module &M, Instruction *inst);
  void addTraceWrite(Module &M);
};

} // namespace

llvm::PassPluginLibraryInfo getMemoryTracePluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, PASS_NAME, LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == PASS_NAME) {
                    MPM.addPass(CLogWave());
                    return true;
                  }

                  return false;
                });
            /* for clang to automatically include this pass */
            /* TODO: why this works? */
            PB.registerPipelineEarlySimplificationEPCallback(
                [&](ModulePassManager &MPM, OptimizationLevel level) {
                  MPM.addPass(CLogWave());
                });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getMemoryTracePluginInfo();
}

PreservedAnalyses CLogWave::run(Module &M, ModuleAnalysisManager &) {
  Function *main_func = M.getFunction("main");
  if (!main_func) {
    log_fatal << "Couldn't find main function in file: " << M.getName() << '\n';
    return PreservedAnalyses::all();
  }
  addFilePtrDeclaration(M);
  addCallToFopen(M, main_func);
  addTraceWrite(M);

  ScopeHierarchy scope_hierarchy;

  for (auto &Func : M) {
    if (!Func.isDeclaration() && Func.getName() != trace_func_name) {
      scope_hierarchy.insert(Func);
    }
  }
  scope_hierarchy.generate_scope();
  return PreservedAnalyses::none();
}

PointerType *get_pointer(llvm::Module &M) {
  return PointerType::getUnqual(Type::getInt8Ty(M.getContext()));
}

/* add a FILE* global var in M's global address space */
void CLogWave::addFilePtrDeclaration(Module &M) {
  M.getOrInsertGlobal(file_ptr_var_name, get_pointer(M));
  GlobalVariable *gbl = M.getNamedGlobal(file_ptr_var_name);
  gbl->setLinkage(GlobalValue::InternalLinkage);
  gbl->setInitializer(llvm::ConstantPointerNull::get(get_pointer(M)));
}

/* Create a global string literal with name argName and initialize it
 * with varName
 */
Constant *createCharPtrArg(llvm::Module &M, StringRef varName,
                           StringRef argName) {
  Constant *val = ConstantDataArray::getString(M.getContext(), argName);
  Constant *var = M.getOrInsertGlobal(varName, val->getType());
  dyn_cast<GlobalVariable>(var)->setInitializer(val);
  return var;
}

/* Add a call to fopen to initialize the FILE * added by addFilePtrDeclaration
 */
void CLogWave::addCallToFopen(Module &M, Function *fmain) {
  auto &ctx = M.getContext();
  Type *fopen_return_type = get_pointer(M);
  std::vector<Type *> fopen_param_type = {get_pointer(M), get_pointer(M)};
  FunctionType *fopen_type =
      FunctionType::get(fopen_return_type, fopen_param_type, false);
  FunctionCallee fopen_calle = M.getOrInsertFunction("fopen", fopen_type);

  /* create fopen args arguments and store them in the program */
  StringRef fopen_arg0_var_name = "gbl_filename";
  StringRef fopen_arg1_var_name = "gbl_mode";
  Constant *fopen_arg0 =
      createCharPtrArg(M, fopen_arg0_var_name, vcd_dump_filename);
  Constant *fopen_arg1 = createCharPtrArg(M, fopen_arg1_var_name, "w+");

  BasicBlock *bb = &fmain->getEntryBlock();
  IRBuilder<> Builder(bb, bb->getFirstInsertionPt());
  llvm::Value *fopen_arg0_char_ptr =
      Builder.CreatePointerCast(fopen_arg0, fopen_param_type[0], "fileNameStr");
  llvm::Value *fopen_arg1_char_ptr =
      Builder.CreatePointerCast(fopen_arg1, fopen_param_type[0], "modeStr");
  llvm::Value *fopen_return = Builder.CreateCall(
      fopen_calle, {fopen_arg0_char_ptr, fopen_arg1_char_ptr});

  GlobalVariable *FPGlobal = M.getNamedGlobal(file_ptr_var_name);
  Builder.CreateStore(fopen_return, FPGlobal);
}

/* Insert a call to fprintf in func */
void CLogWave::insertCallToFprintf(IRBuilder<> &Builder, llvm::Function &func,
                                   llvm::Module &M) {
  auto &ctx = M.getContext();
  std::vector<llvm::Type *> FprintfArgs{
      PointerType::getUnqual(Type::getInt8Ty(ctx)),
      PointerType::getUnqual(Type::getInt8Ty(ctx))};

  FunctionType *FprintfTy =
      FunctionType::get(Type::getInt32Ty(ctx), FprintfArgs, true);

  FunctionCallee Fprintf = M.getOrInsertFunction("fprintf", FprintfTy);

  Constant *hello_str = ConstantDataArray::getString(ctx, "hello world\n");
  Constant *hello_gbl_var = M.getOrInsertGlobal("msg", hello_str->getType());
  dyn_cast<GlobalVariable>(hello_gbl_var)->setInitializer(hello_str);

  GlobalVariable *FPGlobal = M.getNamedGlobal(file_ptr_var_name);
  LoadInst *FP = Builder.CreateLoad(
      PointerType::getUnqual(Type::getInt8Ty(ctx)), FPGlobal);

  GlobalVariable *msg_global = M.getNamedGlobal("msg");
  // LoadInst *msg_load = Builder.CreateLoad(
  // PointerType::getUnqual(Type::getInt8Ty(ctx)), msg_global);

  Builder.CreateCall(Fprintf, {FP, msg_global});
}

/* Add this function into module M
 *    void my_trace() {
 *      fprintf(gbl, "hello");
 *    }
 */
void CLogWave::addTraceWrite(Module &M) {
  auto &ctx = M.getContext();
  FunctionType *my_trace_ty = FunctionType::get(Type::getVoidTy(ctx), false);
  Function *my_trace_func = Function::Create(
      my_trace_ty, GlobalValue::ExternalLinkage, "my_trace", M);
  BasicBlock *first_block = BasicBlock::Create(ctx, "first_bb", my_trace_func);
  IRBuilder<> Builder(first_block);
  Builder.SetInsertPoint(first_block);
  insertCallToFprintf(Builder, *my_trace_func, M);
  Builder.CreateRetVoid();
}

void CLogWave::addTraceToInst(Module &M, Instruction *inst) {
  auto &ctx = M.getContext();
  FunctionType *my_trace_ty = FunctionType::get(Type::getVoidTy(ctx), false);
  FunctionCallee calle = M.getOrInsertFunction("my_trace", my_trace_ty);
  IRBuilder<> Builder(inst->getNextNode());
  Builder.CreateCall(calle);
}
