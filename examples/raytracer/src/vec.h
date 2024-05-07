#pragma once

#include <cmath>
#include <knot/core.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <numeric>

namespace rt {

template <size_t N, typename T>
struct Vec {
  std::array<T, N> elements = {};

  T& operator[](int i) { return elements[i]; }
  T operator[](int i) const { return elements[i]; }

  constexpr static size_t size() { return N; }

  template <typename F>
  friend auto apply(Vec v, F f) {
    Vec<N, decltype(f(T{}))> res;
    std::transform(v.elements.begin(), v.elements.end(), res.elements.begin(), f);
    return res;
  }

  template <typename T2, typename F>
  friend auto binary_apply(Vec a, Vec<N, T2> b, F f) {
    Vec<N, decltype(f(T{}, T2{}))> res;
    for(int i = 0; i < int(N); i++) {
      res.elements[i] = f(a[i], b[i]);
    }
    return res;
  }

  template <typename U>
  friend auto cast(Vec a) {
    return apply(a, [](T x) { return U(x); });
  }

  friend T dot(Vec a, Vec b) {
    const Vec c = a * b;
    return std::accumulate(c.elements.begin(), c.elements.end(), T{0});
  }

  friend T product(Vec v) {
    return std::accumulate(v.elements.begin(), v.elements.end(), T{1}, [](T acc, T ele) { return acc * ele; });
  }

  friend Vec lerp(Vec u, Vec v, T a) { return (T{1} - a) * u + a * v; }

  friend auto clamp(Vec v, T min, T max) {
    return apply(v, [&](T x) { return std::min(std::max(x, min), max); });
  }

  friend auto length(Vec v) { return std::sqrt(dot(v, v)); }

  friend Vec normalized(Vec v) { return v / length(v); }

  friend bool near_zero(Vec v) {
    const auto s = 1e-8;
    return std::fabs(v[0]) < s && std::fabs(v[1]) < s && std::fabs(v[2]) < s;
  }

  Vec<N + 1, T> append(T t) {
    constexpr auto visit = []<size_t... Is>(Vec v, T t, std::index_sequence<Is...>) {
      return Vec<N + 1, T>{v[Is]..., t};
    };
    return visit(*this, t, std::make_index_sequence<N>{});
  }

  friend auto operator+(Vec a, Vec b) {
    return binary_apply(a, b, [](T x, T y) { return x + y; });
  }

  friend auto operator-(Vec a, Vec b) {
    return binary_apply(a, b, [](T x, T y) { return x - y; });
  }

  friend auto operator*(Vec a, Vec b) {
    return binary_apply(a, b, [](T x, T y) { return x * y; });
  }

  friend auto operator/(Vec a, Vec b) {
    return binary_apply(a, b, [](T x, T y) { return x / y; });
  }

  template <typename U>
  friend auto operator*(Vec a, U b) {
    return apply(a, [&](T x) { return x * b; });
  }

  template <typename U>
  friend auto operator*(U a, Vec b) {
    return b * a;
  };

  template <typename U>
  friend auto operator/(Vec a, U b) {
    return apply(a, [&](T x) { return x / b; });
  }

  friend bool operator==(const Vec&, const Vec&) = default;

  friend auto as_tie(Vec v) {
    constexpr auto visit = []<size_t... Is>(Vec v, std::index_sequence<Is...>) { return std::tuple(v[Is]...); };
    return visit(v, std::make_index_sequence<N>{});
  }
};

using Vec2i = Vec<2, int32_t>;
using Vec2 = Vec<2, float>;

using Vec3i = Vec<3, int32_t>;
using Vec3 = Vec<3, float>;

using Color = Vec<3, char>;
using Colorf = Vec3;

inline Vec3 cross(Vec3 a, Vec3 b) {
  return {a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]};
}

inline Vec3 rotate_around_plane(Vec3 normal, Vec3 par, float theta) {
  assert(abs(dot(normal, par)) < 1e-6);
  return normalized(par * std::cos(theta) + cross(normal, par) * std::sin(theta) +
                    normal * dot(normal, par) * (1.0f - std::cos(theta)));
}

} // namespace rt
