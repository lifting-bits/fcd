//
// pre_ast_cfg.h
// Copyright (C) 2015 Félix Cloutier.
// All Rights Reserved.
//
// This file is distributed under the University of Illinois Open Source
// license. See LICENSE.md for details.
//

#ifndef pre_ast_cfg_h
#define pre_ast_cfg_h

#include "not_null.h"

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Function.h>
#include <llvm/Analysis/DominanceFrontier.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Support/GenericDomTree.h>
#include <llvm/Support/GenericDomTreeConstruction.h>

#include "remill/BC/Version.h"

#include <deque>
#include <iterator>
#include <unordered_map>

class AstContext;
class Expression;
struct PreAstBasicBlock;
class PreAstContext;

struct PreAstBasicBlockEdge
{
	PreAstBasicBlock* from;
	PreAstBasicBlock* to;
	Expression* edgeCondition;
	
	PreAstBasicBlockEdge(PreAstBasicBlock& from, PreAstBasicBlock& to, Expression& edgeCondition)
	: from(&from), to(&to), edgeCondition(&edgeCondition)
	{
	}
	
	void setTo(PreAstBasicBlock& newTo);
};

struct PreAstBasicBlock
{
	std::vector<PreAstBasicBlockEdge*> predecessors;
	std::vector<PreAstBasicBlockEdge*> successors;
	
	llvm::BasicBlock* block;
	StatementReference blockStatement;
	PreAstContext* parent;
	
	PreAstBasicBlock() = default;
	PreAstBasicBlock(const PreAstBasicBlock&) = delete;
	PreAstBasicBlock(PreAstBasicBlock&&);
	
	PreAstBasicBlock& operator=(PreAstBasicBlock&&);
	
	void setParent(PreAstContext *ctx) { parent = ctx; }
	PreAstContext* getParent(){ return parent; }
	
	void swap(PreAstBasicBlock& block);
	void printAsOperand(llvm::raw_ostream& os, bool printType);
};

class PreAstContext
{
	AstContext& ctx;
	std::deque<PreAstBasicBlockEdge> edgeList;
	std::deque<PreAstBasicBlock> blockList;
	std::unordered_map<llvm::BasicBlock*, PreAstBasicBlock*> blockMapping;
	
public:
	typedef std::deque<PreAstBasicBlock>::iterator node_iterator;
	
	PreAstContext(AstContext& ctx);
	
	void generateBlocks(llvm::Function& fn);
	PreAstBasicBlock& createRedirectorBlock(llvm::ArrayRef<PreAstBasicBlockEdge*> redirectedEdgeList);

	AstContext& getContext()
	{
		return ctx;
	}
	
	PreAstBasicBlockEdge& createEdge(PreAstBasicBlock& from, PreAstBasicBlock& to, Expression& edgeCondition)
	{
		edgeList.emplace_back(from, to, edgeCondition);
		return edgeList.back();
	}
	
	PreAstBasicBlock& createBlock()
	{
		blockList.emplace_back();
		blockList.back().setParent(this);
		return blockList.back();
	}
	
	PreAstBasicBlock* getEntryBlock()
	{
		return &blockList.front();
	}
	
	node_iterator begin()
	{
		return blockList.begin();
	}
	
	node_iterator end()
	{
		return blockList.end();
	}
	
	size_t size() const
	{
		return blockList.size();
	}
	
	#if LLVM_VERSION_NUMBER >= LLVM_VERSION(4, 0)
	void view() const;
	#endif
};

struct PreAstBasicBlockRegionTraits
{
	typedef PreAstContext FuncT;
	typedef PreAstBasicBlock BlockT;
	typedef llvm::DomTreeNodeBase<PreAstBasicBlock> DomTreeNodeT;
	typedef llvm::ForwardDominanceFrontierBase<PreAstBasicBlock> DomFrontierT;

#if LLVM_VERSION_NUMBER >= LLVM_VERSION(5, 0)
	typedef llvm::DominatorTreeBase<PreAstBasicBlock, false> DomTreeT;
	typedef llvm::DominatorTreeBase<PreAstBasicBlock, true> PostDomTreeT;
#else
	typedef llvm::DominatorTreeBase<PreAstBasicBlock> DomTreeT;
	typedef llvm::DominatorTreeBase<PreAstBasicBlock> PostDomTreeT;
#endif
};

class PABBSuccIter : public std::iterator<std::input_iterator_tag, PreAstBasicBlock*>
{
	private:
		std::vector<PreAstBasicBlockEdge*>::iterator base;
	public:
		PABBSuccIter(std::vector<PreAstBasicBlockEdge*>::iterator iter)
		: base(iter) {}
	
		auto &operator*() const
		{
			return (*base)->to;
		}
		
		auto &operator++()
		{
			++base;
			return *this;
		}
		
		auto operator++(int)
		{
			auto copy = *this;
			++*this;
			return copy;
		}

		auto &operator--()
		{
			--base;
			return *this;
		}

		difference_type operator-(const PABBSuccIter& that) const
		{
			return base - that.base;
		}
	
		bool operator==(const PABBSuccIter& that) const
		{
			return base == that.base;
		}
	
		bool operator!=(const PABBSuccIter& that) const
		{
			return !(base == that.base);
		}
};

class PABBPredIter : public std::iterator<std::input_iterator_tag, PreAstBasicBlock*>
{
	private:
		std::vector<PreAstBasicBlockEdge*>::iterator base;
	public:
		PABBPredIter(std::vector<PreAstBasicBlockEdge*>::iterator iter)
		: base(iter) {}
	
		auto &operator*() const
		{
			return (*base)->from;
		}
		
		auto &operator++()
		{
			++base;
			return *this;
		}
		
		auto operator++(int)
		{
			auto copy = *this;
			++*this;
			return copy;
		}

		auto &operator--()
		{
			--base;
			return *this;
		}

		difference_type operator-(const PABBPredIter& that) const
		{
			return base - that.base;
		}
	
		bool operator==(const PABBPredIter& that) const
		{
			return base == that.base;
		}
	
		bool operator!=(const PABBPredIter& that) const
		{
			return !(base == that.base);
		}
};

class PABBNodesIter : public std::iterator<std::input_iterator_tag, PreAstBasicBlock*>
{
	private:
		std::deque<PreAstBasicBlock>::iterator base;
	public:
		PABBNodesIter(std::deque<PreAstBasicBlock>::iterator iter)
		: base(iter) {}
	
		auto operator*() const
		{
			return &*base;
		}
		
		auto &operator++()
		{
			++base;
			return *this;
		}
		
		auto operator++(int)
		{
			auto copy = *this;
			++*this;
			return copy;
		}

		auto &operator--()
		{
			--base;
			return *this;
		}

		difference_type operator-(const PABBNodesIter& that) const
		{
			return base - that.base;
		}
	
		bool operator==(const PABBNodesIter& that) const
		{
			return base == that.base;
		}
	
		bool operator!=(const PABBNodesIter& that) const
		{
			return !(base == that.base);
		}

		operator PreAstBasicBlock*()
		{
			return operator*();
		}
};

template<>
struct llvm::GraphTraits<PreAstBasicBlock*>
{
	typedef PreAstBasicBlock NodeType;
	typedef PreAstBasicBlock* NodeRef;
	typedef PABBSuccIter ChildIteratorType;
	
	static NodeRef getEntryNode(PreAstBasicBlock* block)
	{
		return block;
	}
	
	static ChildIteratorType child_begin(NodeType *node)
	{
		return ChildIteratorType(node->successors.begin());
	}
	
	static ChildIteratorType child_end(NodeType *node)
	{
		return ChildIteratorType(node->successors.end());
	}
};

template<>
struct llvm::GraphTraits<llvm::Inverse<PreAstBasicBlock*>>
{
	typedef PreAstBasicBlock NodeType;
	typedef PreAstBasicBlock* NodeRef;
	typedef PABBPredIter ChildIteratorType;
	
	static NodeRef getEntryNode(PreAstBasicBlock* block)
	{
		return block;
	}
	
	static ChildIteratorType child_begin(NodeRef node)
	{
		return ChildIteratorType(node->predecessors.begin());
	}
	
	static ChildIteratorType child_end(NodeRef node)
	{
		return ChildIteratorType(node->predecessors.end());
	}
};

struct PreAstContextGraphTraits
{
#if LLVM_VERSION_NUMBER >= LLVM_VERSION(4, 0) || LLVM_VERSION_NUMBER <= LLVM_VERSION(3, 7)
	typedef PABBNodesIter nodes_iterator;
#else
	typedef std::deque<PreAstBasicBlock>::iterator nodes_iterator;
#endif
	
	static nodes_iterator nodes_begin(PreAstContext* f)
	{
#if LLVM_VERSION_NUMBER >= LLVM_VERSION(4, 0) || LLVM_VERSION_NUMBER <= LLVM_VERSION(3, 7)
		return PABBNodesIter(f->begin());
#else
		return f->begin();
#endif
	}
	
	static nodes_iterator nodes_end(PreAstContext* f)
	{
#if LLVM_VERSION_NUMBER >= LLVM_VERSION(4, 0) || LLVM_VERSION_NUMBER <= LLVM_VERSION(3, 7)
		return PABBNodesIter(f->end());
#else
		return f->end();
#endif
	}
	
	static size_t size(PreAstContext* f)
	{
		return f->size();
	}
	
	static PreAstBasicBlock* getEntryNode(PreAstContext* context)
	{
		return context->getEntryBlock();
	}
};

template<>
struct llvm::GraphTraits<PreAstContext*>
: public llvm::GraphTraits<PreAstBasicBlock*>, public PreAstContextGraphTraits
{
	using llvm::GraphTraits<PreAstBasicBlock*>::getEntryNode;
	using PreAstContextGraphTraits::getEntryNode;
};

template<>
struct llvm::GraphTraits<llvm::Inverse<PreAstContext*>>
: public llvm::GraphTraits<Inverse<PreAstBasicBlock*>>, public PreAstContextGraphTraits
{
	using llvm::GraphTraits<Inverse<PreAstBasicBlock*>>::getEntryNode;
	using PreAstContextGraphTraits::getEntryNode;
};

#if LLVM_VERSION_NUMBER > LLVM_VERSION(3, 7)

template<>
struct llvm::GraphTraits<PreAstBasicBlockRegionTraits::DomTreeNodeT*>
: public llvm::DomTreeGraphTraitsBase<PreAstBasicBlockRegionTraits::DomTreeNodeT, PreAstBasicBlockRegionTraits::DomTreeNodeT::iterator>
{
};

template<>
struct llvm::GraphTraits<const PreAstBasicBlockRegionTraits::DomTreeNodeT*>
: public llvm::DomTreeGraphTraitsBase<const PreAstBasicBlockRegionTraits::DomTreeNodeT, PreAstBasicBlockRegionTraits::DomTreeNodeT::const_iterator>
{
};

#endif

#endif /* pre_ast_cfg_hpp */
