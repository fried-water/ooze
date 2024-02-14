#pragma once

#include "ooze/iter.h"
#include "ooze/primitives.h"
#include "ooze/strong_id.h"

#include <knot/core.h>

#include <numeric>
#include <type_traits>
#include <vector>

namespace ooze {

template <typename T, typename ID = i32>
class Forest {
  constexpr static ID INVALID = ID(-1);

  static_assert(std::is_default_constructible_v<T>);

  template <typename F, typename Traits>
  class Iter : public ForwardIter<Iter<F, Traits>, typename Traits::value_type> {
    using forest_type = std::conditional_t<Traits::is_const, const Forest, Forest>;

    forest_type* _forest = nullptr;
    ID _id = INVALID;

  public:
    Iter() = default;
    Iter(forest_type* forest, ID id) : _forest{forest}, _id{id} {}

    auto& deref() const { return Traits{}(*_forest, _id); }
    bool eq(const Iter& rhs) const { return _id == rhs._id; }
    void increment() { _id = F{}(*_forest, _id); }
  };

  struct IDIterTraits {
    constexpr static bool is_const = true;
    using value_type = const ID;
    value_type& operator()(const Forest&, const ID& id) const { return id; }
  };

  template <bool IsConst>
  struct ValueIterTraits {
    constexpr static bool is_const = IsConst;
    using value_type = std::conditional_t<IsConst, const T, T>;
    template <typename F>
    value_type& operator()(F& f, const ID& id) const {
      return f[id];
    }
  };

  struct AllIDs {
    ID operator()(const Forest&, ID id) const { return ID(as_integral(id) + 1); };
  };

  struct SiblingIDs {
    ID operator()(const Forest& f, ID id) const { return f._connectivity[as_integral(id)].next_sibling; };
  };

  struct AncestorIDs {
    ID operator()(const Forest& f, ID id) const { return f._connectivity[as_integral(id)].parent; };
  };

  struct PreOrderIDs {
    ID operator()(const Forest& f, ID id) const {
      const ID child = f._connectivity[as_integral(id)].first_child;
      return child != INVALID ? child : f.next_pre_order_sibling(id);
    }
  };

  struct PostOrderIDs {
    ID operator()(const Forest& f, ID id) const {
      const ID sibling = f._connectivity[as_integral(id)].next_sibling;
      return sibling != INVALID ? f.first_leaf(sibling) : f._connectivity[as_integral(id)].parent;
    }
  };

  struct LeafIDs {
    ID operator()(const Forest& f, ID id) const {
      do {
        id = PreOrderIDs{}(f, id);
      } while(id != INVALID && !f.is_leaf(id));
      return id;
    }
  };

  struct Connectivity {
    ID parent = INVALID;
    ID next_sibling = INVALID;
    ID first_child = INVALID;
  };

  std::vector<T> _values;
  std::vector<Connectivity> _connectivity;
  ID _first_root = INVALID;

  static std::optional<ID> opt(ID id) { return id != INVALID ? std::optional(id) : std::nullopt; }

  void disconnect(ID id) {
    const ID parent = _connectivity[as_integral(id)].parent;
    ID& child_ref = parent == INVALID ? _first_root : _connectivity[as_integral(parent)].first_child;

    if(child_ref == id) {
      child_ref = _connectivity[as_integral(id)].next_sibling;
    } else {
      const auto siblings = parent == INVALID ? root_ids() : child_ids(parent);
      const ID prev_sibling = *std::find_if(siblings.begin(), siblings.end(), [&](ID sibling) {
        return _connectivity[as_integral(sibling)].next_sibling == id;
      });

      _connectivity[as_integral(prev_sibling)].next_sibling = _connectivity[as_integral(id)].next_sibling;
    }
  }

  ID first_leaf_or_invalid(ID id) const { return id != INVALID ? first_leaf(id) : INVALID; }

public:
  using AllIDIter = Iter<AllIDs, IDIterTraits>;
  using AllIter = Iter<AllIDs, ValueIterTraits<false>>;
  using AllConstIter = Iter<AllIDs, ValueIterTraits<true>>;

  using SiblingIDIter = Iter<SiblingIDs, IDIterTraits>;
  using SiblingIter = Iter<SiblingIDs, ValueIterTraits<false>>;
  using SiblingConstIter = Iter<SiblingIDs, ValueIterTraits<true>>;

  using AncestorIDIter = Iter<AncestorIDs, IDIterTraits>;
  using AncestorIter = Iter<AncestorIDs, ValueIterTraits<false>>;
  using AncestorConstIter = Iter<AncestorIDs, ValueIterTraits<true>>;

  using PreOrderIDIter = Iter<PreOrderIDs, IDIterTraits>;
  using PreOrderIter = Iter<PreOrderIDs, ValueIterTraits<false>>;
  using PreOrderConstIter = Iter<PreOrderIDs, ValueIterTraits<true>>;

  using PostOrderIDIter = Iter<PostOrderIDs, IDIterTraits>;
  using PostOrderIter = Iter<PostOrderIDs, ValueIterTraits<false>>;
  using PostOrderConstIter = Iter<PostOrderIDs, ValueIterTraits<true>>;

  using LeafIDIter = Iter<LeafIDs, IDIterTraits>;
  using LeafIter = Iter<LeafIDs, ValueIterTraits<false>>;
  using LeafConstIter = Iter<LeafIDs, ValueIterTraits<true>>;

  constexpr static ID ABOVE_ROOTS = ID(-2);

  Forest() = default;

  template <typename U, typename F>
  Forest(const Forest<U, ID>& other, F f) : _connectivity(other._connectivity), _first_root(other._first_root) {
    _values.reserve(other._values.size());
    std::transform(other._values.begin(), other._values.end(), std::back_inserter(_values), f);
  }

  const T& operator[](ID id) const { return _values[as_integral(id)]; }
  T& operator[](ID id) { return _values[as_integral(id)]; }

  void reserve(size_t s) {
    _values.reserve(s);
    _connectivity.reserve(s);
  }

  void shrink_to_fit() {
    _values.shrink_to_fit();
    _connectivity.shrink_to_fit();
  }

  i64 ssize() const { return i64(_values.size()); }
  size_t size() const { return _values.size(); }

  size_t num_children(ID id) const { return child_ids(id).size(); }
  size_t num_roots() const { return root_ids().size(); }

  bool is_root(ID id) const { return _connectivity[as_integral(id)].parent == INVALID; }
  bool is_leaf(ID id) const { return _connectivity[as_integral(id)].first_child == INVALID; }

  std::optional<ID> first_root() const { return opt(_first_root); }
  std::optional<ID> parent(ID id) const { return opt(_connectivity[as_integral(id)].parent); }
  std::optional<ID> next_sibling(ID id) const { return opt(_connectivity[as_integral(id)].next_sibling); }
  std::optional<ID> first_child(ID id) const { return opt(_connectivity[as_integral(id)].first_child); }

  ID root(ID id) const {
    ID parent = _connectivity[as_integral(id)].parent;
    while(parent != INVALID) {
      id = parent;
      parent = _connectivity[as_integral(id)].parent;
    }

    return id;
  }

  ID first_leaf(ID id) const {
    while(_connectivity[as_integral(id)].first_child != INVALID) {
      id = _connectivity[as_integral(id)].first_child;
    }
    return id;
  }

  ID next_pre_order_sibling(ID id) const {
    while(id != INVALID) {
      if(ID sibling = _connectivity[as_integral(id)].next_sibling; sibling != INVALID) {
        return sibling;
      }
      id = _connectivity[as_integral(id)].parent;
    }

    return INVALID;
  }

  /// Modifiers

  ID append_root(T value) { return append_child(ABOVE_ROOTS, std::move(value)); }

  ID append_child(ID parent, T value) {
    const ID id = ID(_values.size());

    ID* slot = parent == ABOVE_ROOTS ? &_first_root : &_connectivity[as_integral(parent)].first_child;
    while(*slot != INVALID) {
      slot = &_connectivity[as_integral(*slot)].next_sibling;
    }
    *slot = id;

    _values.push_back(std::move(value));
    _connectivity.push_back({parent == ABOVE_ROOTS ? INVALID : parent, INVALID, INVALID});

    return id;
  }

  void move_first_child(ID parent, ID child) {
    disconnect(child);

    _connectivity[as_integral(child)].next_sibling = _connectivity[as_integral(parent)].first_child;
    _connectivity[as_integral(child)].parent = parent;
    _connectivity[as_integral(parent)].first_child = child;
  }

  template <typename IDs>
  ID append_root_post_order(T value, const IDs& ids) {
    const ID id = append_child(ABOVE_ROOTS, std::move(value));
    if(!ids.empty()) {
      for(ID child : ids) disconnect(child);
      for(ID child : ids) _connectivity[as_integral(child)].parent = id;

      _connectivity[as_integral(id)].first_child = ids.front();
      for(int i = 1; i < ids.size(); i++) {
        _connectivity[as_integral(ids[i - 1])].next_sibling = ids[i];
      }
      _connectivity[as_integral(ids.back())].next_sibling = INVALID;
    }
    return id;
  }

  void pop_last_root() {
    const ID root = ID(_values.size() - 1);
    assert(is_root(root));

    ID* slot = &_first_root;
    while(*slot != root) slot = &_connectivity[as_integral(*slot)].next_sibling;
    *slot = _connectivity[as_integral(root)].first_child;

    ID child = _connectivity[as_integral(root)].first_child;
    while(child != INVALID) {
      _connectivity[as_integral(child)].parent = INVALID;
      child = _connectivity[as_integral(child)].next_sibling;
      ;
    }

    _values.pop_back();
    _connectivity.pop_back();
  }

  template <typename Range>
  ID append_path(ID id, const Range& range) {
    return std::accumulate(range.begin(), range.end(), id, [&](ID id, const auto& ele) {
      return append_child(id, ele);
    });
  }

  template <typename Range>
  ID merge_path(ID id, const Range& range) {
    for(auto it = range.begin(); it != range.end(); ++it) {
      const auto children = id == ABOVE_ROOTS ? root_ids() : child_ids(id);
      const auto find_it = std::find_if(children.begin(), children.end(), [&](ID id) { return (*this)[id] == *it; });

      if(find_it != children.end()) {
        id = *find_it;
      } else {
        id = append_path(id, IterRange(it, range.end()));
        break;
      }
    }

    return id;
  }

  template <typename Range>
  ID merge_path(const Range& range) {
    return merge_path(ABOVE_ROOTS, range);
  }

  /// traversals

  auto ids() const { return IterRange(AllIDIter(this, ID(0)), AllIDIter(this, ID(_values.size()))); }

  auto values() { return IterRange(AllIter(this, ID(0)), AllIter(this, ID(_values.size()))); }

  auto values() const { return IterRange(AllConstIter(this, ID(0)), AllConstIter(this, ID(_values.size()))); }

  auto child_ids(ID id) const {
    return IterRange(SiblingIDIter(this, id == ABOVE_ROOTS ? _first_root : _connectivity[as_integral(id)].first_child),
                     SiblingIDIter());
  }

  auto children(ID id) {
    return IterRange(SiblingIter(this, id == ABOVE_ROOTS ? _first_root : _connectivity[as_integral(id)].first_child),
                     SiblingIter());
  }

  auto children(ID id) const {
    return IterRange(
      SiblingConstIter(this, id == ABOVE_ROOTS ? _first_root : _connectivity[as_integral(id)].first_child),
      SiblingConstIter());
  }

  auto root_ids() const { return child_ids(ABOVE_ROOTS); }
  auto roots() { return children(ABOVE_ROOTS); }
  auto roots() const { return children(ABOVE_ROOTS); }

  auto ancestor_ids(ID id, bool include_self = false) const {
    return IterRange(AncestorIDIter(this, include_self ? id : _connectivity[as_integral(id)].parent), AncestorIDIter());
  }

  auto ancestors(ID id, bool include_self = false) {
    return IterRange(AncestorIter(this, include_self ? id : _connectivity[as_integral(id)].parent), AncestorIter());
  }

  auto ancestors(ID id, bool include_self = false) const {
    return IterRange(AncestorIter(this, include_self ? id : _connectivity[as_integral(id)].parent), AncestorIter());
  }

  auto pre_order_ids(ID id = ABOVE_ROOTS) const {
    return IterRange(PreOrderIDIter(this, id == ABOVE_ROOTS ? _first_root : id),
                     PreOrderIDIter(this, id == ABOVE_ROOTS ? INVALID : next_pre_order_sibling(id)));
  }

  auto pre_order(ID id = ABOVE_ROOTS) {
    return IterRange(PreOrderIter(this, id == ABOVE_ROOTS ? _first_root : id),
                     PreOrderIter(this, id == ABOVE_ROOTS ? INVALID : next_pre_order_sibling(id)));
  }

  auto pre_order(ID id = ABOVE_ROOTS) const {
    return IterRange(PreOrderConstIter(this, id == ABOVE_ROOTS ? _first_root : id),
                     PreOrderConstIter(this, id == ABOVE_ROOTS ? INVALID : next_pre_order_sibling(id)));
  }

  auto post_order_ids(ID id = ABOVE_ROOTS) const {
    return IterRange(
      PostOrderIDIter(
        this, id == ABOVE_ROOTS ? (_first_root != INVALID ? first_leaf(_first_root) : INVALID) : first_leaf(id)),
      PostOrderIDIter(this, id == ABOVE_ROOTS ? INVALID : PostOrderIDs{}(*this, id)));
  }

  auto post_order(ID id = ABOVE_ROOTS) {
    return IterRange(
      PostOrderIter(this,
                    id == ABOVE_ROOTS ? (_first_root != INVALID ? first_leaf(_first_root) : INVALID) : first_leaf(id)),
      PostOrderIter(this, id == ABOVE_ROOTS ? INVALID : PostOrderIDs{}(*this, id)));
  }

  auto post_order(ID id = ABOVE_ROOTS) const {
    return IterRange(
      PostOrderConstIter(
        this, id == ABOVE_ROOTS ? (_first_root != INVALID ? first_leaf(_first_root) : INVALID) : first_leaf(id)),
      PostOrderConstIter(this, id == ABOVE_ROOTS ? INVALID : PostOrderIDs{}(*this, id)));
  }

  auto leaf_ids(ID id = ABOVE_ROOTS) const {
    return IterRange(LeafIDIter(this, id == ABOVE_ROOTS ? first_leaf_or_invalid(_first_root) : first_leaf(id)),
                     LeafIDIter(this, id == ABOVE_ROOTS ? INVALID : first_leaf_or_invalid(next_pre_order_sibling(id))));
  }

  auto leaves(ID id = ABOVE_ROOTS) {
    return IterRange(LeafIter(this, id == ABOVE_ROOTS ? first_leaf_or_invalid(_first_root) : first_leaf(id)),
                     LeafIter(this, id == ABOVE_ROOTS ? INVALID : first_leaf_or_invalid(next_pre_order_sibling(id))));
  }

  auto leaves(ID id = ABOVE_ROOTS) const {
    return IterRange(
      LeafConstIter(this,
                    id == ABOVE_ROOTS ? (_first_root != INVALID ? first_leaf(_first_root) : INVALID) : first_leaf(id)),
      LeafConstIter(this, id == ABOVE_ROOTS ? INVALID : first_leaf_or_invalid(next_pre_order_sibling(id))));
  }

  friend bool compare_trees(const Forest& f1, const Forest& f2, ID lhs, ID rhs) {
    const auto lhs_children = f1.child_ids(lhs);
    const auto rhs_children = f2.child_ids(rhs);

    return f1[lhs] == f2[rhs] &&
           std::equal(
             lhs_children.begin(), lhs_children.end(), rhs_children.begin(), rhs_children.end(), [&](ID x, ID y) {
               return compare_trees(f1, f2, x, y);
             });
  }

  friend auto as_tie(const Forest& f) { return std::tie(f._connectivity, f._values, f._first_root); }

  friend bool operator==(const Forest& f1, const Forest& f2) {
    const auto lhs_roots = f1.root_ids();
    const auto rhs_roots = f2.root_ids();

    return std::equal(lhs_roots.begin(), lhs_roots.end(), rhs_roots.begin(), rhs_roots.end(), [&](ID x, ID y) {
      return compare_trees(f1, f2, x, y);
    });
  }

  friend bool operator!=(const Forest& f1, const Forest& f2) { return !(f1 == f2); }
};

} // namespace ooze
