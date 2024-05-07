#pragma once

#include "vec.h"

#include <memory>
#include <optional>
#include <span>

namespace rt {

struct Image {
  // Assume RGB
  constexpr static int BYTES_PER_PIXEL = 3;

  static size_t size_in_bytes(Vec2i v) { return product(v) * Image::BYTES_PER_PIXEL; }

  Vec2i dims;
  std::unique_ptr<std::byte[]> data;

  auto span() { return std::span(reinterpret_cast<Color*>(data.get()), product(dims)); }

  Color& operator[](Vec2i p) { return span()[p[1] * dims[0] + p[0]]; }
};

Image create_empty_image(Vec2i);

int write_png(const Image&, const char*);
std::optional<Image> read(const char*);

} // namespace rt
