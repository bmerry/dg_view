/*
   This file is part of Datagrind, a tool for tracking data accesses.

   Copyright (C) 2010 Bruce Merry
      bmerry@users.sourceforge.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef DG_VIEW_H
#define DG_VIEW_H

#include <cstddef>
#include <stdint.h>
#include <vector>
#include <utility>
#include <string>
#include <set>
#include <map>
#include "dg_view_pool.h"
#include "dg_view_range.h"
#include "dg_view_options.h"
#include "dg_view_base.h"

/* TODO: get from the guest */
#define DG_VIEW_PAGE_SIZE 4096
#define DG_VIEW_LINE_SIZE 64

/* Subclass containing the implementation, templatised by the word size. */
template<typename WordType>
class dg_view : public dg_view_base
{
private:
    class bbrun;
    class bbdef;
    class bbdef_access;
    class context;

    typedef WordType word_type;

    class iterator_data : public iterator_data_base
    {
    public:
        const dg_view<WordType> *owner;
        std::size_t bbrun_index;
        std::size_t addr_index;

    protected:
        virtual iterator_data_base *clone() const;
        virtual void increment();

    private:
        const bbrun &get_bbrun() const;
        const context &get_context() const;
        const bbdef &get_bbdef() const;
        const bbdef_access &get_bbdef_access() const;

    public:
        virtual bool operator==(const iterator_data_base &b) const;

        virtual address_type get_addr() const;
        virtual iseq_type get_iseq() const;
        virtual size_t get_size() const;
        virtual uint8_t get_dir() const;
        virtual std::vector<address_type> get_stack() const;

        /* Information about the memory block accessed */
        virtual address_type get_mem_addr() const;
        virtual address_type get_mem_size() const;
        virtual std::vector<address_type> get_mem_stack() const;
        virtual std::string get_mem_label() const;

        virtual ~iterator_data() {}
    };

    /* Memory block allocated with malloc or similar function in the guest */
    struct mem_block
    {
        word_type addr;
        word_type size;
        std::vector<word_type> stack;
        std::string label;
        bool matched;      /* Matches the requirements of -m, if any */
    };

    struct context
    {
        uint64_t bbdef_index;
        std::vector<word_type> stack;
    };

    struct bbdef_access
    {
        uint8_t dir;
        uint8_t size;
        uint8_t iseq;
    };

    struct bbdef
    {
        std::vector<word_type> instr_addrs;
        std::vector<bbdef_access> accesses;
    };

    struct bbrun
    {
        uint64_t iseq_start;
        uint64_t dseq_start;
        uint32_t context_index;
        uint32_t n_addrs;
        word_type *addrs;            /* Allocated from word_pool; NULL means discarded */
        mem_block **blocks;          /* Allocated from mem_block_ptr_pool */
    };

    typedef std::vector<bbrun> bbrun_list;

    struct compare_bbrun_iseq
    {
        bool operator()(const bbrun &a, const bbrun &b) const
        {
            return a.iseq_start < b.iseq_start;
        }

        bool operator()(const bbrun &a, uint64_t iseq) const
        {
            return a.iseq_start < iseq;
        }

        bool operator()(uint64_t iseq, const bbrun &a) const
        {
            return iseq < a.iseq_start;
        }
    };

    /* Wrapper around an options condition_set with a cache for lookups */
    class condition_set
    {
    private:
        mutable std::map<word_type, bool> cache;

    public:
        const dg_view_options::condition_set &base;

        condition_set(const dg_view_options::condition_set &base) : base(base) {}
        /* Checks whether a condition specified by a condition_set is met. It
         * does not consider user events or the flags.
         */
        bool match(const std::vector<word_type> &stack) const;
    };

    typedef std::map<address_type, size_t> forward_page_map;
    typedef std::map<size_t, address_type> reverse_page_map;

    const bbdef &bbrun_get_bbdef(const bbrun &bbr) const;

public:
    virtual const_iterator begin() const;
    virtual const_iterator end() const;
    virtual void get_ranges(address_type &addr_min, address_type &addr_max, iseq_type &iseq_min, iseq_type &iseq_max) const;
    virtual const_iterator nearest_access(double addr, double iseq, double ratio) const;

    virtual forward_page_map_iterator page_map_begin() const;
    virtual forward_page_map_iterator page_map_end() const;
    virtual address_type revmap_addr(std::size_t addr) const;
    virtual std::size_t remap_address(address_type a) const;

private:
    pool_allocator<word_type> word_pool;
    pool_allocator<mem_block *> mem_block_ptr_pool;

    /* All START_EVENTs with no matching END_EVENT from chosen_events */
    std::multiset<std::string> active_events;
    /* All TRACK_RANGEs with no matching UNTRACK_RANGE from chosen_ranges */
    std::multiset<std::pair<word_type, word_type> > active_ranges;

    rangemap<word_type, mem_block *> block_map;
    std::vector<mem_block *> block_storage;

    /* Events selected on the command line */
    condition_set event_conditions;
    /* Ranges selected on the command line */
    condition_set block_conditions;

    std::vector<bbdef> bbdefs;
    std::vector<bbrun> bbruns;
    std::vector<context> contexts;
    forward_page_map fwd_page_map;
    reverse_page_map rev_page_map;

    /* Internal helper methods */
    std::pair<double, std::size_t> nearest_access_bbrun(const bbrun &bbr, double addr, double iseq, double ratio) const;

    mem_block *find_block(word_type addr) const;

    /* Whether a memory access matches the range conditions */
    bool keep_access_block(word_type addr, uint8_t size, mem_block *block) const;

    /* Whether a memory access matches the event conditions */
    bool keep_access_event(const std::vector<word_type> &stack) const;

    /* Whether a memory access matches the selection criteria */
    bool keep_access(word_type addr, uint8_t size, const std::vector<word_type> &stack, mem_block *block) const;

    dg_view(FILE *f, const std::string &filename, int version, int endian, const dg_view_options &options);
    friend dg_view_base *dg_view_load(const std::string &filename, const dg_view_options &options);
};

dg_view_base *dg_view_load(const std::string &filename, const dg_view_options &options);

#endif /* DG_VIEW_H */
