#include "Function.h"
#include "Unit.h"
#include "Type.h"
#include <list>
#include <queue>

extern FILE *yyout;

Function::Function(Unit *u, SymbolEntry *s)
{
    u->insertFunc(this);
    entry = new BasicBlock(this);
    sym_ptr = s;
    parent = u;
}

Function::~Function()
{
    //     auto delete_list = block_list;
    //     for (auto &i : delete_list)
    //         delete i;
    //     parent->removeFunc(this);
}

// remove the basicblock bb from its block_list.
void Function::remove(BasicBlock *bb)
{
    auto temp = std::find(block_list.begin(), block_list.end(), bb);
    block_list.erase(temp);
}

void Function::output() const {
    FunctionType* funcType = dynamic_cast<FunctionType*>(sym_ptr->getType());
    Type *retType = funcType->getRetType();
    std::queue<BasicBlock*> q;
    std::set<BasicBlock*> v;
    fprintf(yyout, "define %s %s(",retType->toStr().c_str(), sym_ptr->toStr().c_str());
    int i = 0;
    for(auto it : params)
    {
        fprintf(yyout, "%s %s", it->getType()->toStr().c_str(),it->toStr().c_str());
        if(i != (&params)->size() - 1)
        {
            fprintf(yyout,",");
        }
        i++;
    }
    fprintf(yyout,") {\n");
    v.insert(entry);
    for (q.push(entry); !q.empty(); ) {
        BasicBlock* bb = q.front();
        q.pop();
        bb->output();
        std::vector<BasicBlock*>::iterator succ;
        succ = bb->succ_begin();
        while (succ != bb->succ_end()) {
            if (v.find(*succ) == v.end()) {
                v.insert(*succ);
                q.push(*succ);
            }
            succ++;
        }
    }
    fprintf(yyout, "}\n");
}

void Function::genMachineCode(AsmBuilder* builder) 
{
    auto cur_func = new MachineFunction(builder->getUnit(), this->sym_ptr);
    if(!Isleaf())
    {
        cur_func->setUnleaf();
    }
    builder->setFunction(cur_func);
    std::map<BasicBlock*, MachineBlock*> map;
    for(auto block : block_list)
    {
        block->genMachineCode(builder);
        map[block] = builder->getBlock();
    }
    // Add pred and succ for every block
    for(auto block : block_list)
    {
        for (auto pred = block->pred_begin(); pred != block->pred_end(); pred++)
            map[block]->addPred(map[*pred]);
        for (auto succ = block->succ_begin(); succ != block->succ_end(); succ++)
            map[block]->addSucc(map[*succ]);
    }
    auto cur_unit = builder->getUnit();
    cur_unit->InsertFunc(cur_func);
}
