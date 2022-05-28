#include "pch.h"

#include "env.h"

namespace ooze {

Env create_primative_env() {
  Env env;

  env.add<bool>("bool");

  env.add<i8>("i8");
  env.add<i16>("i16");
  env.add<i32>("i32");
  env.add<i64>("i64");

  env.add<u8>("u8");
  env.add<u16>("u16");
  env.add<u32>("u32");
  env.add<u64>("u64");

  env.add<f32>("f32");
  env.add<f64>("f64");

  env.add<std::string>("string");

  return env;
}

} // namespace ooze
