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

#ifndef __STDC_FORMAT_MACROS
    #define __STDC_FORMAT_MACROS 1
#endif
#include <inttypes.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cassert>
#include <stdint.h>
#include <vector>
#include <algorithm>
#include <set>
#include <map>
#include <string>
#include <memory>
#include <iterator>
#include <sstream>
#include <typeinfo>

#include "dg_record.h"
#include "dg_view.h"
#include "dg_view_range.h"
#include "dg_view_debuginfo.h"
#include "dg_view_parse.h"
#include "dg_view_pool.h"

using namespace std;

template<typename T>
static T page_round_down(T x)
{
    return x & ~(DG_VIEW_PAGE_SIZE - 1);
}

template<typename WordType>
dg_view_base::iterator_data_base *dg_view<WordType>::iterator_data::clone() const
{
    iterator_data *n = new iterator_data;
    n->owner = owner;
    n->bbrun_index = bbrun_index;
    n->addr_index = addr_index;
    return n;
}

template<typename WordType>
void dg_view<WordType>::iterator_data::increment()
{
    assert(bbrun_index < owner->bbruns.size());
    addr_index++;
    if (addr_index == owner->bbruns[bbrun_index].n_addrs)
    {
        addr_index = 0;
        bbrun_index++;
    }
}

template<typename WordType>
bool dg_view<WordType>::iterator_data::operator==(const iterator_data_base &b) const
{
    if (typeid(b) != typeid(*this))
        return false;
    else
    {
        const iterator_data &o = static_cast<const iterator_data &>(b);
        return owner == o.owner
            && bbrun_index == o.bbrun_index
            && addr_index == o.addr_index;
    }
}

template<typename WordType>
const typename dg_view<WordType>::bbrun &dg_view<WordType>::iterator_data::get_bbrun() const
{
    assert(bbrun_index < owner->bbruns.size());
    const bbrun &bb = owner->bbruns[bbrun_index];
    assert(addr_index < bb.n_addrs);
    return bb;
}

template<typename WordType>
const typename dg_view<WordType>::context &dg_view<WordType>::iterator_data::get_context() const
{
    const bbrun &bb = get_bbrun();
    assert(bb.context_index < owner->contexts.size());
    return owner->contexts[bb.context_index];
}

template<typename WordType>
const typename dg_view<WordType>::bbdef &dg_view<WordType>::iterator_data::get_bbdef() const
{
    const bbrun &bb = get_bbrun();
    const bbdef &bbd = owner->bbrun_get_bbdef(bb);
    assert(addr_index < bbd.accesses.size());
    return bbd;
}

template<typename WordType>
const typename dg_view<WordType>::bbdef_access &dg_view<WordType>::iterator_data::get_bbdef_access() const
{
    return get_bbdef().accesses[addr_index];
}

template<typename WordType>
address_type dg_view<WordType>::iterator_data::get_addr() const
{
    return get_bbrun().addrs[addr_index];
}

template<typename WordType>
iseq_type dg_view<WordType>::iterator_data::get_iseq() const
{
    return get_bbrun().iseq_start + get_bbdef_access().iseq;
}

template<typename WordType>
size_t dg_view<WordType>::iterator_data::get_size() const
{
    return get_bbdef_access().size;
}

template<typename WordType>
uint8_t dg_view<WordType>::iterator_data::get_dir() const
{
    return get_bbdef_access().dir;
}

template<typename WordType>
vector<address_type> dg_view<WordType>::iterator_data::get_stack() const
{
    vector<address_type> ans;
    const context &ctx = get_context();
    const bbdef &bbd = get_bbdef();
    ans.push_back(bbd.instr_addrs[bbd.accesses[addr_index].iseq]);
    copy(ctx.stack.begin(), ctx.stack.end(), back_inserter(ans));
    return ans;
}

template<typename WordType>
address_type dg_view<WordType>::iterator_data::get_mem_addr() const
{
    const bbrun &bb = get_bbrun();
    const mem_block *block = bb.blocks[addr_index];
    return block ? block->addr : 0;
}

template<typename WordType>
address_type dg_view<WordType>::iterator_data::get_mem_size() const
{
    const bbrun &bb = get_bbrun();
    const mem_block *block = bb.blocks[addr_index];
    return block ? block->size : 0;
}

template<typename WordType>
vector<address_type> dg_view<WordType>::iterator_data::get_mem_stack() const
{
    vector<address_type> ans;
    const bbrun &bb = get_bbrun();
    const mem_block *block = bb.blocks[addr_index];
    if (block != NULL)
    {
        copy(block->stack.begin(), block->stack.end(), back_inserter(ans));
    }
    return ans;
}

template<typename WordType>
string dg_view<WordType>::iterator_data::get_mem_label() const
{
    const bbrun &bb = get_bbrun();
    const mem_block *block = bb.blocks[addr_index];
    return block ? block->label : string();
}

template<typename WordType>
dg_view_base::const_iterator dg_view<WordType>::begin() const
{
    iterator_data *data = new iterator_data;
    data->owner = this;
    data->bbrun_index = 0;
    data->addr_index = 0;
    return const_iterator(data);
}

template<typename WordType>
dg_view_base::const_iterator dg_view<WordType>::end() const
{
    iterator_data *data = new iterator_data;
    data->owner = this;
    data->bbrun_index = bbruns.size();
    data->addr_index = 0;
    return const_iterator(data);
}

/* ratio is the ratio of address scale to iseq scale: a large value for ratio
 * increases the importance of the address in the match.
 *
 * Returns the best score and best index for the block. If there were no
 * usable addresses, returns score of HUGE_VAL;
 */
template<typename WordType>
pair<double, size_t> dg_view<WordType>::nearest_access_bbrun(const bbrun &bbr, double addr, double iseq, double ratio) const
{
    double best_score = HUGE_VAL;
    size_t best_i = 0;

    const context &ctx = contexts[bbr.context_index];
    const bbdef &bbd = bbdefs[ctx.bbdef_index];
    for (size_t i = 0; i < bbr.n_addrs; i++)
        if (bbr.addrs[i])
        {
            double addr_score = (bbr.addrs[i] - addr) * ratio;
            uint64_t cur_iseq = bbr.iseq_start + bbd.accesses[i].iseq;
            double score = hypot(addr_score, cur_iseq - iseq);
            if (score < best_score)
            {
                best_score = score;
                best_i = i;
            }
        }
    return make_pair(best_score, best_i);
}

template<typename WordType>
dg_view_base::const_iterator dg_view<WordType>::nearest_access(double addr, double iseq, double ratio) const
{
    /* Start at the right instruction and search outwards until we can bound
     * the search.
     */
    typename vector<bbrun>::const_iterator back, forw, best = bbruns.end();
    size_t best_i = 0;
    double best_score = HUGE_VAL;

    forw = lower_bound(bbruns.begin(), bbruns.end(), (uint64_t) iseq, compare_bbrun_iseq());
    back = forw;
    best = forw;
    while (forw != bbruns.end() || back != bbruns.begin())
    {
        if (forw != bbruns.end())
        {
            if (forw->iseq_start > iseq + best_score)
                forw = bbruns.end();
            else
            {
                pair<double, size_t> sub = nearest_access_bbrun(*forw, addr, iseq, ratio);
                if (sub.first < best_score)
                {
                    best_score = sub.first;
                    best_i = sub.second;
                    best = forw;
                }
                forw++;
            }
        }
        if (back != bbruns.begin())
        {
            if (back->iseq_start <= iseq - best_score)
                back = bbruns.begin();
            else
            {
                --back;
                pair<double, size_t> sub = nearest_access_bbrun(*back, addr, iseq, ratio);
                if (sub.first < best_score)
                {
                    best_score = sub.first;
                    best_i = sub.second;
                    best = back;
                }
            }
        }
    }

    auto_ptr<iterator_data> ans(new iterator_data());

    ans->owner = this;
    ans->bbrun_index = best - bbruns.begin();
    ans->addr_index = best_i;
    return iterator(ans.release());
}

template<typename WordType>
const typename dg_view<WordType>::bbdef &dg_view<WordType>::bbrun_get_bbdef(const bbrun &bbr) const
{
    assert(bbr.context_index < contexts.size());
    const context &ctx = contexts[bbr.context_index];
    assert(ctx.bbdef_index < bbdefs.size());
    return bbdefs[ctx.bbdef_index];
}

template<typename WordType>
typename dg_view<WordType>::mem_block *dg_view<WordType>::find_block(word_type addr) const
{
    mem_block *block = NULL;
    typename rangemap<word_type, mem_block *>::const_iterator block_it = block_map.find(addr);
    if (block_it != block_map.end())
        block = block_it->second;
    return block;
}

/* Checks whether a condition specified by a condition_set is met. It does not
 * include user events or the flags.
 */
template<typename WordType>
bool dg_view<WordType>::condition_set::match(const vector<word_type> &stack) const
{
    if (base.functions.empty() && base.files.empty() && base.dsos.empty())
        return false;
    for (size_t i = 0; i < stack.size(); i++)
    {
        typename map<word_type, bool>::iterator pos = cache.lower_bound(stack[i]);
        if (pos != cache.end() && pos->first == stack[i])
        {
            if (pos->second)
                return true;
            else
                continue;
        }

        bool match = false;
        string function, file, dso;
        int line;
        dg_view_addr2info(stack[i], function, file, line, dso);
        if ((!function.empty() && base.functions.count(function))
            || (!file.empty() && base.files.count(file))
            || (!dso.empty() && base.dsos.count(dso)))
        {
            match = true;
        }
        cache.insert(pos, make_pair(stack[i], match));
        if (match)
            return true;
    }

    return false;
}

/* Whether a memory access matches the range conditions */
template<typename WordType>
bool dg_view<WordType>::keep_access_block(word_type addr, uint8_t size, mem_block *block) const
{
    if (block_conditions.base.flags & CONDITION_SET_FLAG_ANY)
        return true;
    if (block != NULL && block->matched)
        return true;

    for (typename multiset<pair<word_type, word_type> >::const_iterator i = active_ranges.begin(); i != active_ranges.end(); ++i)
        if (addr + size > i->first && addr < i->first + i->second)
            return true;

    return false;
}

/* Whether a memory access matches the event conditions */
template<typename WordType>
bool dg_view<WordType>::keep_access_event(const vector<word_type> &stack) const
{
    if (event_conditions.base.flags & CONDITION_SET_FLAG_ANY)
        return true;
    if (!active_events.empty())
        return true;

    return event_conditions.match(stack);
}

template<typename WordType>
bool dg_view<WordType>::keep_access(word_type addr, uint8_t size, const vector<word_type> &stack, mem_block *block) const
{
    return keep_access_block(addr, size, block) && keep_access_event(stack);
}

template<typename WordType>
void dg_view<WordType>::get_ranges(address_type &addr_min, address_type &addr_max, iseq_type &iseq_min, iseq_type &iseq_max) const
{
    addr_min = fwd_page_map.begin()->second;
    addr_max = (--fwd_page_map.end())->second + DG_VIEW_PAGE_SIZE;
    iseq_min = bbruns.begin()->iseq_start;

    typename bbrun_list::const_iterator last_bbrun = --bbruns.end();
    const bbdef &last_bbdef = bbrun_get_bbdef(*last_bbrun);
    iseq_max = last_bbrun->iseq_start + last_bbdef.accesses.back().iseq + 1;
}

template<typename WordType>
size_t dg_view<WordType>::remap_address(address_type a) const
{
    word_type base = page_round_down(a);
    forward_page_map::const_iterator it = fwd_page_map.find(base);
    assert(it != fwd_page_map.end());
    return (a - base) + it->second;
}

template<typename WordType>
dg_view_base::forward_page_map_iterator dg_view<WordType>::page_map_begin() const
{
    return fwd_page_map.begin();
}

template<typename WordType>
dg_view_base::forward_page_map_iterator dg_view<WordType>::page_map_end() const
{
    return fwd_page_map.end();
}

template<typename WordType>
address_type dg_view<WordType>::revmap_addr(size_t addr) const
{
    word_type remapped_page = page_round_down(addr);
    typename reverse_page_map::const_iterator pos = rev_page_map.find(remapped_page);
    if (pos == rev_page_map.end())
        return 0;
    word_type page = pos->second;
    word_type addr2 = (addr - remapped_page) + page;
    return addr2;
}

template<typename WordType>
dg_view<WordType>::dg_view(FILE *f, const std::string &filename, int version, int endian, const dg_view_options &options) :
    event_conditions(options.get_event_conditions()),
    block_conditions(options.get_block_conditions())
{
    uint64_t iseq = 0;
    uint64_t dseq = 0;
    record_parser *rp_ptr;

    while (NULL != (rp_ptr = record_parser::create(f)))
    {
        auto_ptr<record_parser> rp(rp_ptr);
        uint8_t type = rp->get_type();

        try
        {
            switch (type)
            {
            case DG_R_HEADER:
                throw record_parser_content_error("Error: found header after first record.\n");

            case DG_R_BBDEF:
                {
                    bbdef bbd;
                    uint8_t n_instrs = rp->extract_byte();
                    word_type n_accesses = rp->extract_word<word_type>();

                    if (n_instrs == 0)
                    {
                        throw record_parser_content_error("Error: empty BB");
                    }
                    bbd.instr_addrs.resize(n_instrs);
                    bbd.accesses.resize(n_accesses);

                    for (word_type i = 0; i < n_instrs; i++)
                    {
                        bbd.instr_addrs[i] = rp->extract_word<word_type>();
                        // discard size
                        (void) rp->extract_byte();
                    }
                    for (word_type i = 0; i < n_accesses; i++)
                    {
                        bbd.accesses[i].dir = rp->extract_byte();
                        bbd.accesses[i].size = rp->extract_byte();
                        bbd.accesses[i].iseq = rp->extract_byte();
                        if (bbd.accesses[i].iseq >= n_instrs)
                        {
                            throw record_parser_content_error("iseq is greater than instruction count");
                        }
                    }
                    bbdefs.push_back(bbd);
                }
                break;
            case DG_R_CONTEXT:
                {
                    context ctx;
                    ctx.bbdef_index = rp->extract_word<word_type>();

                    uint8_t n_stack = rp->extract_byte();
                    if (n_stack == 0)
                        throw record_parser_content_error("Error: empty call stack");
                    ctx.stack.resize(n_stack);
                    for (uint8_t i = 0; i < n_stack; i++)
                        ctx.stack[i] = rp->extract_word<word_type>();

                    if (ctx.bbdef_index >= bbdefs.size())
                    {
                        ostringstream msg;
                        msg << "Error: bbdef index " << ctx.bbdef_index << " is out of range";
                        throw record_parser_content_error(msg.str());
                    }
                    contexts.push_back(ctx);
                }
                break;
            case DG_R_BBRUN:
                {
                    bbrun bbr;
                    bool keep_any = false;

                    bbr.iseq_start = iseq;
                    bbr.dseq_start = dseq;
                    bbr.context_index = rp->extract_word<word_type>();
                    if (bbr.context_index >= contexts.size())
                    {
                        ostringstream msg;
                        msg << "Error: context index " << bbr.context_index << " is out of range";
                        throw record_parser_content_error(msg.str());
                    }

                    const context &ctx = contexts[bbr.context_index];
                    const bbdef &bbd = bbdefs[ctx.bbdef_index];
                    uint8_t n_instrs = rp->extract_byte();
                    uint64_t n_addrs = rp->remain() / sizeof(word_type);
                    if (n_addrs > bbd.accesses.size())
                        throw record_parser_content_error("Error: too many access addresses");

                    bbr.n_addrs = n_addrs;
                    bbr.addrs = word_pool.alloc(n_addrs);
                    bbr.blocks = mem_block_ptr_pool.alloc(n_addrs);
                    vector<word_type> stack = ctx.stack;
                    for (word_type i = 0; i < n_addrs; i++)
                    {
                        word_type addr = rp->extract_word<word_type>();
                        const bbdef_access &access = bbd.accesses[i];

                        mem_block *block = find_block(addr);
                        stack[0] = bbd.instr_addrs[access.iseq];
                        bool keep = keep_access(addr, access.size, stack, block);
                        if (keep)
                        {
                            keep_any = true;
                            fwd_page_map[page_round_down(addr)] = 0;
                            bbr.addrs[i] = addr;
                            bbr.blocks[i] = block;
                        }
                        else
                        {
                            bbr.addrs[i] = 0;
                            bbr.blocks[i] = NULL;
                        }
                    }

                    if (keep_any)
                        bbruns.push_back(bbr);
                    iseq += n_instrs;
                    dseq += n_addrs;
                }
                break;
            case DG_R_TRACK_RANGE:
                {
                    word_type addr = rp->extract_word<word_type>();
                    word_type size = rp->extract_word<word_type>();

                    string var_type = rp->extract_string();
                    string label = rp->extract_string();

                    if (block_conditions.base.user.count(label)
                        || (block_conditions.base.flags & CONDITION_SET_FLAG_RANGE))
                        active_ranges.insert(make_pair(addr, size));
                }
                break;
            case DG_R_UNTRACK_RANGE:
                {
                    word_type addr = rp->extract_word<word_type>();
                    word_type size = rp->extract_word<word_type>();

                    pair<word_type, word_type> key(addr, size);
                    typename multiset<pair<word_type, word_type> >::iterator it = active_ranges.find(key);
                    if (it != active_ranges.end())
                        active_ranges.erase(it);
                }
                break;
            case DG_R_MALLOC_BLOCK:
                {
                    word_type addr = rp->extract_word<word_type>();
                    word_type size = rp->extract_word<word_type>();
                    word_type n_ips = rp->extract_word<word_type>();
                    vector<word_type> ips;

                    mem_block *block = new mem_block;
                    block->addr = addr;
                    block->size = size;
                    block->stack.reserve(n_ips);
                    for (word_type i = 0; i < n_ips; i++)
                    {
                        word_type stack_addr = rp->extract_word<word_type>();
                        block->stack.push_back(stack_addr);
                    }
                    block->matched = false;
                    if (block_conditions.base.flags & (CONDITION_SET_FLAG_ANY | CONDITION_SET_FLAG_MALLOC)
                        || block_conditions.match(block->stack))
                        block->matched = true;
                    block_storage.push_back(block);
                    block_map.insert(addr, addr + size, block);
                }
                break;
            case DG_R_FREE_BLOCK:
                {
                    word_type addr = rp->extract_word<word_type>();
                    block_map.erase(addr);
                }
                break;
            case DG_R_START_EVENT:
            case DG_R_END_EVENT:
                {
                    string label = rp->extract_string();
                    if (event_conditions.base.user.count(label))
                    {
                        if (type == DG_R_START_EVENT)
                            active_events.insert(label);
                        else
                        {
                            multiset<string>::iterator it = active_events.find(label);
                            if (it != active_events.end())
                                active_events.erase(it);
                        }
                    }
                }
                break;
            case DG_R_TEXT_AVMA:
                {
                    word_type avma = rp->extract_word<word_type>();
                    string object_filename = rp->extract_string();
                    dg_view_load_object_file(object_filename.c_str(), avma);
                }
                break;
            default:
                {
                    ostringstream msg;
                    msg << showbase << hex;
                    msg << "Error: unknown record type " << (unsigned int) type;
                    throw record_parser_content_error(msg.str());
                }
            }
            rp->finish();
        }
        catch (record_parser_content_error &e)
        {
            fprintf(stderr, "%s: %s\n", filename.c_str(), e.what());
            rp->discard();
        }
    }

    /* bbruns is easily the largest structure, and due to the way vectors
     * work, could be overcommitted. Shrink back to just fit. */
    vector<bbrun> tmp(bbruns.begin(), bbruns.end());
    bbruns.swap(tmp);

    size_t remapped_base = 0;
    for (forward_page_map::iterator i = fwd_page_map.begin(); i != fwd_page_map.end(); i++)
    {
        i->second = remapped_base;
        remapped_base += DG_VIEW_PAGE_SIZE;
        rev_page_map[i->second] = i->first;
    }

    if (bbruns.empty())
    {
        throw record_parser_error("No accesses match the criteria.");
    }

#if 1
    printf("  %zu bbdefs\n"
           "  %zu bbruns\n"
           "  %zu contexts\n"
           "  %" PRIu64 " instrs (approx)\n"
           "  %" PRIu64 " accesses\n",
           bbdefs.size(),
           bbruns.size(),
           contexts.size(),
           bbruns.back().iseq_start,
           bbruns.back().dseq_start + bbruns.back().n_addrs);
#endif
}

dg_view_base *dg_view_load(const std::string &filename, const dg_view_options &options)
{
    record_parser *rp_ptr;
    dg_view_base *accesses;
    FILE *f = fopen(filename.c_str(), "rb");
    if (!f)
    {
        fprintf(stderr, "Could not open `%s'.\n", filename.c_str());
        return NULL;
    }

    try
    {
        uint8_t version, endian, wordsize;
        if (NULL != (rp_ptr = record_parser::create(f)))
        {
            auto_ptr<record_parser> rp(rp_ptr);
            uint8_t type = rp->get_type();
            uint8_t expected_version = 1;

            if (type != DG_R_HEADER)
                throw record_parser_error("Error: did not find header");
            if (rp->extract_string() != "DATAGRIND1")
                throw record_parser_error("Error: did not find signature");
            version = rp->extract_byte();
            endian = rp->extract_byte();
            wordsize = rp->extract_byte();
            if (version != expected_version)
            {
                fprintf(stderr, "Warning: version mismatch (expected %d, got %u).\n",
                        expected_version, version);
            }
            /* TODO: do something useful with endianness */

            if (wordsize != 4 && wordsize != 8)
            {
                ostringstream msg;
                msg << "Error: unsupported word size (got " << wordsize << ", expected 4 or 8)";
                throw record_parser_error(msg.str());
            }
        }
        else
        {
            throw record_parser_error("Error: empty or unreadable file");
        }

        switch (wordsize)
        {
        case 4:
            accesses = new dg_view<uint32_t>(f, filename, version, endian, options);
            break;
        case 8:
            accesses = new dg_view<uint64_t>(f, filename, version, endian, options);
            break;
        }
    }
    catch (record_parser_error &e)
    {
        fprintf(stderr, "%s: %s\n", filename.c_str(), e.what());
        fclose(f);
        return NULL;
    }
    catch (...)
    {
        fclose(f);
        throw;
    }

    fclose(f);
    return accesses;
}

template class dg_view<uint32_t>;
template class dg_view<uint64_t>;
