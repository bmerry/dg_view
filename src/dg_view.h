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
#include "dg_view_rangemap.h"

/* TODO: get from the guest */
#define DG_VIEW_PAGE_SIZE 4096
#define DG_VIEW_LINE_SIZE 64

/* Abstract base class, which holds data about an input file. It it subclassed
 * based on the word size of the file.
 */
class dg_view_base
{
private:
    /* Abstract base class for the data in an iterator. This is separated
     * out from the iterator class itself so that the iterator class can
     * be non-virtual.
     */
    class iterator_data_base
    {
        /* Creates an identical copy in new memory */
        virtual iterator_data *clone() const = 0;

        /* Moves on to the next access */
        virtual void increment() = 0;

        virtual ~iterator_data() {}

        virtual address_type get_addr() const = 0;
        virtual iseq_type get_iseq() const = 0;
        virtual size_t get_size() const = 0; /* Size of the access */
        virtual uint8_t get_dir() const = 0; /* One of the DG_ACC_* tokens */
        virtual std::vector<address_type> get_stack() const = 0;

        /* Information about the memory block accessed */
        virtual address_type get_mem_addr() const = 0;
        virtual address_type get_mem_size() const = 0; /* 0 if no information available */
        virtual std::vector<address_type> get_mem_stack() const = 0;
        virtual std::string get_mem_label() const = 0;
    };

public:
    typedef uint64_t address_type;
    typedef uint64_t iseq_type;

    /* Iterator over the data accesses. This is a wrapper class that
     * wraps an instance of a subclass of iterator_data.
     */
    class iterator
    {
        iterator_data *data; /* Actually a pointer to a subclass */

        iterator() : data(NULL) {}
        iterator(const iterator &it) : data(NULL)
        {
            if (it.data != NULL)
                data = it.data->clone();
        }

        ~iterator() { delete data; }

        iterator &operator=(const iterator &it)
        {
            delete data;
            if (it.data != NULL)
            {
                data = it.data->clone();
            }
            else
            {
                data = NULL;
            }
            return *this;
        }

        iterator &operator++()
        {
            data->increment();
            return *this;
        }

        iterator operator++(int)
        {
            iterator old(*this);
            data->increment();
            return old;
        }

        const iterator_data *operator

        bool operator==(const iterator &it) const
        {
            if (data == NULL && it.data == NULL)
            {
                return true;
            }
            else if (data != NULL && it.data != NULL)
            {
                return *data == *it.data;
            }
            else
            {
                return false;
            }
        }

        bool operator!=(const iterator &it) const
        {
            return !(*this == it);
        }
    };

    typedef iterator const_iterator;

protected:
    friend dg_view_base *dg_view_load(const char *filename);

public:
    virtual const_iterator begin() const = 0;
    virtual const_iterator end() const = 0;
    virtual void get_ranges(address_type &addr_min, address_type &addr_max, iseq_type &iseq_min, iseq_type &iseq_max) const = 0;

    /* ratio is the ratio of address scale to iseq scale: a large value for ratio
     * increases the importance of the address in the match.
     *
     * Returns the best score and best index for the block. If there were no
     * usable addresses, returns score of HUGE_VAL.
     */
    virtual const_iterator nearest_access(double addr, double iseq, double ratio) const = 0;
};

template<typename WordType>
class dg_view : public dg_view_base
{
private:
    typedef iterator_data : public iterator_data_base
    {
        dg_view<WordType> *owner;
        std::size_t bbrun_index;
        std::size_t addr_index;

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
    };

private:
    typedef WordType word_type;

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
        word_type *addrs;             /* Allocated from word_pool; NULL means discarded */
        mem_block<Word> **blocks;     /* Allocated from mem_block_ptr_pool */
    };

    typedef std::map<word_type, size_t> forward_page_map;
    typedef std::map<size_t, word_type> reverse_page_map;
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

    /* Set of conditions to match stack traces against based on command-line options */
    struct condition_set
    {
        uint32_t flags;         /* set-specific flags, not generically used */
        std::set<std::string> user;
        std::set<std::string> functions;
        std::set<std::string> files;
        std::set<std::string> dsos;
        mutable std::map<word_type, bool> cache; /* cache for condition_set_match */
    };

    const bbdef &bbrun_get_bbdef(const bbrun &bbr) const;

public:
    virtual const_iterator begin() const;
    virtual const_iterator end() const;
    virtual void get_ranges(address_type &addr_min, address_type &addr_max, iseq_type &iseq_min, iseq_type &iseq_max) const;
    virtual const_iterator nearest_access(double addr, double iseq, double ratio) const;

    void parse_opts(int *argc, char **argv);
    void usage(const char *prog, int code) const;
    bool load(const char *filename);

    /* Takes a compressed address and maps it to an original one */
    word_type revmap_addr(std::size_t addr) const;
    /* Maps original address to compressed address */
    std::size_t remap_address(word_type a) const;

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

    /* Checks whether a condition specified by a condition_set is met. It does not
     * include user events or the flags.
     */
    bool condition_set_match(const condition_set &cs, const std::vector<word_type> &stack) const;

    /* Whether a memory access matches the range conditions */
    bool keep_access_block(word_type addr, uint8_t size, mem_block *block) const;

    /* Whether a memory access matches the event conditions */
    bool keep_access_event(const std::vector<word_type> &stack) const;

    bool keep_access(word_type addr, uint8_t size, const std::vector<word_type> &stack, mem_block *block) const;
};

#endif /* DG_VIEW_H */
