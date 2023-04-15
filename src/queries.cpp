#include "pch.h"

#include "queries.h"

namespace ooze {

namespace {

template <typename Range, typename F>
std::string join(const Range& range, F f) {
  if(range.begin() == range.end()) {
    return "()";
  } else {
    std::string r = "(" + f(*range.begin());
    std::for_each(range.begin() + 1, range.end(), [&](const auto& ele) { r += ", " + f(ele); });
    return r + ")";
  }
}

struct TypeStrVisitor {
  const Env& e;

  std::string operator()(Floating) const { return "_"; }
  std::string operator()(TypeID t) const { return type_name_or_id(e, t); }
  std::string operator()(Borrow<TypeID> b) const { return fmt::format("&{}", (*this)(*b.type)); }

  std::string operator()(FunctionType<TypeID> v) const {
    return fmt::format("{} -> {}", (*this)(*v.input), (*this)(*v.output));
  }

  std::string operator()(const std::vector<CompoundType<TypeID>>& v) const { return join(v, *this); }
  std::string operator()(const CompoundType<TypeID>& t) const { return std::visit(*this, t.v); }
};

} // namespace

std::string type_string(const Env& e, const CompoundType<TypeID>& t) { return TypeStrVisitor{e}(t); }

std::string function_string(const Env& e, std::string_view fn_name, const EnvFunction& f) {
  return fmt::format("{}{}", fn_name, type_string(e, f.type));
}

} // namespace ooze
