===================
YAML File Hierarchy
===================

Hierarchy of YAML configuration files
-------------------------------------

The following graph illustrates the hierarchy of the different YAML configuration files. 

.. graphviz::
    :name: yaml hierarchy
    :caption: ESM-Tools configuration files hierarchy
    :alt: ESM-Tools configuration files hierarchy
    :align: center

     digraph "file_hierarchy" {
         size="6,4";
         graph [fontname="Verdana", fontsize="12"];
         node [fontname="Verdana", fontsize="12"];
         edge [fontname="Sans", fontsize="12"];
         rankdir="TB";
         runscript [label="<runscript>.yaml", shape="note", fillcolor="gray", style=filled];
         component_file [label="<component>.yaml", shape="note", fontcolor=black, fillcolor="gray", style=filled];
         setup_file [label="<setup>.yaml", shape="note", fillcolor=gray, style=filled];
         machine_file [label="<machine>.yaml", shape="note", fillcolor=gray, style=filled];
         user [label="User"];

         component_file -> machine_file [label=" overwrites "];
         setup_file -> component_file [label=" overwrites "];
         runscript -> setup_file [label=" overwrites "];
         user -> runscript [label=" edits "];
         developer [label="Developer"];
         developer -> machine_file [label=" edits "];
         developer -> setup_file [label=" edits "];
         developer -> component_file [label=" edits "];

     }

