#pragma once

#include "ooze/forest.h"

#include <functional>
#include <ostream>
#include <string>

namespace ooze {

struct GlobalDotOptions {
  std::string graph_layout = "TB";

  // Edge options
  std::string edge_color = "black";
  std::string edge_style = "solid";
  bool use_arrows = true;
};

struct NodeDotOptions {
  std::string label;
  std::string shape = "ellipse";
  std::string fill_color = "lightblue";
  std::string font_name = "Arial";
  int font_size = 12;
};

template <typename T, typename ID, typename NodeOptionsCallback>
void generate_dot(const Forest<T, ID>& forest,
                  std::ostream& os,
                  NodeOptionsCallback node_options_func,
                  const GlobalDotOptions& global_options = {}) {

  os << "digraph ForestGraph {\n"
     << "\trankdir=" << global_options.graph_layout << ";\n"
     << "\tedge"
     << " [color=" << global_options.edge_color << ", style=" << global_options.edge_style
     << ", dir=" << (global_options.use_arrows ? "forward" : "none") << "];\n";

  // Add nodes
  for(const ID id : forest.ids()) {
    const NodeDotOptions& options = node_options_func(forest[id], id);
    os << "\t" << id << " [label=\"" << options.label << "\", fontname=" << options.font_name << ", fontsize="
       << options.font_size << ", color=" << options.fill_color << ", shape=" << options.shape << "];\n";
  }

  // Add Edges
  for(const ID id : forest.ids()) {
    for(const ID child_id : forest.child_ids(id)) {
      os << "\t" << id << " -> " << child_id << ";\n";
    }
  }

  os << "}\n";
}

} // namespace ooze
