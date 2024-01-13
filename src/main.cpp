#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#define PASS_NAME "CLogWave"
#define log_fatal errs() << PASS_NAME << ": FATAL: "

#if 0
1. module pass
2. print all important store instructions
3. design DS for storing variables
4. insert print func to the module
#endif

using namespace llvm;

namespace {

class FuncContainer {
  llvm::StringRef name; 
};

class VarContainer {

};

class Node {
  std::vector<VarContainer*> vars;
  std::vector<FuncContainer*> funcs;
};

class ScopeHierarchy {

};

struct CLogWave : public llvm::PassInfoMixin<CLogWave> {
  const StringRef file_ptr_var_name = "MemTraceFilePtr";
  const StringRef vcd_dump_filename = "clogwave_dump.vcd";
  const StringRef trace_func_name = "my_trace";
  int store_cnt = 0;
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &);
  void addFilePtrDeclaration(llvm::Module &M);
  void addCallToFopen(Module &M, Function *fmain);
  void insertCallToFprintf(IRBuilder<> &Builder, llvm::Function &func, llvm::Module &M);
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

  for (auto &Func : M) {
    outs() << "In function: " << Func.getName() << '\n';
    for (auto &BB : Func) {
      for (auto Inst = BB.begin(); Inst != BB.end(); ++Inst) {
        if (isa<StoreInst>(Inst)) {
          StoreInst *inst = dyn_cast<StoreInst>(Inst);
          outs() << '\t' << inst << '\n';
          addTraceToInst(M, inst);
        }
        else if (isa<CallInst>(Inst)) {
          CallInst *inst = dyn_cast<CallInst>(Inst);
          Function *fn = inst->getCalledFunction();
          if (fn) {
            if (fn->getName() != "my_trace" && !fn->isDeclaration()) {
              outs() << '\t' << "My child: " << fn->getName() << '\n';
            }
          }
        }
      }
    }
  }
  outs() << "Total stores: " << store_cnt << '\n';
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

/* Add a call to fopen to initialize the FILE * added by addFilePtrDeclaration */
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
  //LoadInst *msg_load = Builder.CreateLoad( PointerType::getUnqual(Type::getInt8Ty(ctx)), msg_global);

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
  Function *my_trace_func = Function::Create(my_trace_ty, GlobalValue::ExternalLinkage, "my_trace", M);
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
