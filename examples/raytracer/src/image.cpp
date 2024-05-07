#include "image.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image.h"
#include "stb_image_write.h"
#pragma clang diagnostic pop

#include <cstring>

namespace rt {

Image create_empty_image(Vec2i dims) { return Image{dims, std::make_unique<std::byte[]>(Image::size_in_bytes(dims))}; }

int write_png(const Image& img, const char* filename) {
  return stbi_write_png(
    filename, img.dims[0], img.dims[1], Image::BYTES_PER_PIXEL, img.data.get(), img.dims[0] * Image::BYTES_PER_PIXEL);
}

std::optional<Image> read(const char* filename) {
  Vec2i dims;
  int channels;
  unsigned char* img = stbi_load(filename, &dims[0], &dims[1], &channels, 0);
  if(img != nullptr && channels == Image::BYTES_PER_PIXEL) {
    const auto size = Image::size_in_bytes(dims);
    std::unique_ptr<std::byte[]> buffer = std::make_unique<std::byte[]>(size);
    memcpy(buffer.get(), img, size);
    stbi_image_free(img);
    return Image{dims, std::move(buffer)};
  } else {
    return std::nullopt;
  }
}

} // namespace rt
