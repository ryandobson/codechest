# ARCHITECTURE

## Programmatic Construction of Latent Variable Models

---

## 1. Purpose and Scope

This repository implements a **programmatic system for constructing,
modifying, and comparing latent variable models**, with a primary focus on:

- Measurement invariance (configural, metric, scalar, residual)
- Partial measurement invariance workflows
- Automated identification strategies
- Systematic manipulation of lavaan model syntax

The system operates on **lavaan model syntax as a mutable, composable object**, 
allowing statistical intent to be expressed declaratively rather
than through manual string editing.

This document defines the **architectural principles** that govern the
codebase and must be respected when extending or modifying it.

---

## 2. Core Philosophy

### 2.1 The system is a compiler

This codebase functions as a **domain-specific compiler**:

- **Input:** Statistical intent (constraints, identification,
  freed parameters)
- **Intermediate representation:** Structured lavaan syntax
- **Output:** A valid SEM specification

Functions are evaluated by how clearly and safely they move intent through
this pipeline.

Domain-specific compiler means that it takes in higher-level instructs (e.g.,
constrain metric invariance across items 4-8), and then turns those instructions
into the low-level lavaan syntax necessary. 

---

### 2.2 Meaning is separate from representation

A central invariant:

> **Statistical meaning must never be encoded directly in syntax
> generation.**

- Orchestrators decide *what* constraints exist
- Setters apply *where* constraints apply
- Generators decide *how* constraints are written
- The label engine decides *parameter identity*

Violations of this separation create brittle, unextendable code.

---

## 3. Four-Layer Architecture

### Layer 1 — Orchestration (Statistical Intent)

**Examples**

- `set_constraints()`
- `configural_constraints()`
- `metric_constraints()`
- `scalar_constraints()`
- `pmi_*()` workflows

**Responsibilities**

- Define statistical goals
- Determine invariance level
- Control sequencing of constraint application
- Coordinate model comparison

**Prohibited**

- Writing lavaan syntax
- Constructing parameter labels
- Iterating over syntax blocks

**Rule**

> Orchestrators may change. Engines must not.

---

### Layer 2 — Setters (Edit Passes)

**Examples**

- `set_group_loadings()`
- `set_group_intercepts()`
- `set_group_variances()`
- `set_group_means()`

**Responsibilities**

- Apply one class of constraints
- Ensure idempotence (remove → generate → insert)
- Loop over structural dimensions (e.g., groups)

**Prohibited**

- Statistical interpretation
- Label construction logic
- Invariance semantics

**Rule**

> Setters are mechanical editors.

---

### Layer 3 — Generators (Syntax Writers)

**Examples**

- `.generate_loadings()`
- `.generate_intercepts()`
- `.generate_residuals()`
- `.generate_covariances()`

**Responsibilities**

- Convert abstract rules into lavaan lines
- Loop over variables or items
- Delegate identity decisions to the label engine

**Allowed**

- Optional namespacing (e.g., `group_index`)
- Support for new constraint forms

**Prohibited**

- Group or time semantics
- Invariance logic
- Model comparison decisions

**Rule**

> Generators answer “how is this written?”

---

### Layer 4 — Parameter Label Engine (Identity Semantics)

**Example**

- `.parameter_label_engine()`

**Responsibilities**

- Decide whether parameters are:
  - free or fixed
  - equal or unique
  - effects-coded or marker-identified
- Generate consistent labels
- Enforce identification constraints

**Rule**

> All parameter identity logic lives here or nowhere.

---

## 4. Supporting Infrastructure

### 4.1 Removal Helpers

**Examples**

- `.remove_loadings()`
- `.remove_intercepts()`
- `.remove_residuals()`

**Purpose**

- Guarantee idempotence
- Prevent conflicting syntax accumulation

---

### 4.2 Dimension Mappers

**Examples**

- `.map_group_blocks()`
- `.parse_groups()`

**Purpose**

- Iterate over structural dimensions
- Maintain block boundaries

**Principle**

> Dimensions are structural, not semantic.

---

## 5. Canonical Processing Flow

1. Orchestrator defines intent  
2. Setter removes existing syntax  
3. Generator writes new syntax  
4. Label engine assigns identity  
5. Setter inserts syntax  
6. Identification constraints applied if required  

This flow must remain deterministic, composable, and reversible.

---

## 6. Architectural Invariants

The following must never be violated:

1. **Idempotence** – repeated application yields the same model  
2. **Purity** – no side effects or global state  
3. **Late binding** – labels are generated from rules, not stored  
4. **Orthogonality** – dimensions (group, time, level) are independent  

---

## 7. Extension Principles

### 7.1 Longitudinal Measurement Invariance

- Time is treated as an additional structural dimension
- Generators remain unchanged
- Dimension mappers and orchestrators handle time indexing

---

### 7.2 Multilevel Models

- Levels are treated as namespaces or blocks
- Identification and constraint logic remains centralized
- New orchestrators may be added without engine refactors

---

### 7.3 New Constraint Types

When adding new constraint families:

- Extend the label engine or add generators
- Do not branch existing generators
- Preserve idempotence and purity

---

## 8. Public API Philosophy

- Public functions express statistical intent
- Internal functions may change freely
- Errors should explain *why*, not just *what*

---

## 9. Development Checklist

Before adding new functionality:

1. Can this be added without rewriting generators?
2. Does it introduce a new dimension?
3. Does it preserve idempotence?
4. Does it keep meaning out of syntax generation?
5. Can it be removed cleanly?

If not, redesign.

---

## 10. Guiding Statement

> **This system prioritizes clarity of intent, separation of concerns,
> and extensibility over short-term convenience.**









BLOCK MAPPING ARCHITECTURE

OVERVIEW

This package provides a system for programmatic construction and
transformation of lavaan model syntax. Instead of manually editing
model strings, users specify statistical intent (e.g., measurement
invariance constraints), which the package compiles into valid SEM
syntax.

A core challenge in this approach is applying transformations
consistently and safely across different structural contexts, such as:

- multi-group models (group: blocks)
- multilevel models (level: blocks)
- single-group models
- future extensions (e.g., informants, method blocks)

To address this, the package adopts a block mapping architecture that
separates where a transformation applies from what the transformation
does.


WHAT IS A BLOCK?

A block is a contiguous section of lavaan syntax that shares a common
structural context.

Examples include:

- a group: block in multi-group SEM
- a level: block in multilevel SEM
- the entire model in a single-group analysis

Blocks are syntactic units, not statistical concepts. They exist to
localize transformations, not to encode meaning.


THE THREE COMPONENTS OF BLOCK MAPPING

Block mapping is built from three distinct components:

1. Parsers
2. The mapper
3. Callbacks (FUN)

Each component has a single responsibility.


PARSERS: DEFINING BLOCK BOUNDARIES

A parser takes a model string and returns a list of blocks. Each block
contains:

- the syntax lines belonging to that block
- metadata (indices) describing the block’s structural context

Parsers do not modify model content.

Conceptually, a group parser converts:

group: Male
  x =~ x1 + x2

group: Female
  x =~ x1 + x2

into a structured representation:

list(
  list(
    lines   = c("group: Male", "x =~ x1 + x2"),
    indices = list(group = 1, group_name = "Male")
  ),
  list(
    lines   = c("group: Female", "x =~ x1 + x2"),
    indices = list(group = 2, group_name = "Female")
  )
)

Design rule:
Parsers define structure, not statistical meaning.
They do not decide which parameters are constrained or how
identification is handled.


THE MAPPER: ITERATING OVER BLOCKS

The mapper (.map_blocks) performs iteration only.

It:
- receives blocks from a parser
- applies a callback (FUN) to each block
- reassembles the transformed blocks into a model string

Conceptually:

.map_blocks <- function(model_string, parser, FUN) {
  blocks <- parser(model_string)

  for (i in seq_along(blocks)) {
    blocks[[i]]$lines <- FUN(
      block   = blocks[[i]]$lines,
      indices = blocks[[i]]$indices
    )
  }

  assemble_model(blocks)
}

Design rule:
.map_blocks must never interpret block meaning.
It should not know whether a block corresponds to a group, a level,
or any other dimension.


CALLBACKS (FUN): TRANSFORMING BLOCK CONTENTS

The callback defines what transformation is applied to a block.

It receives:
- the block’s syntax lines
- metadata (indices) describing the block’s context

Example:

FUN <- function(block, indices) {
  block <- remove_intercepts(block)
  c(block, generate_intercepts(group = indices$group))
}

Callbacks modify content, not structure.
They never locate blocks or reassemble the model.


ASSEMBLING THE MODEL

After all blocks have been transformed, they are recombined into a
single lavaan model string.

This is handled by a dedicated assembler function.

assemble_model <- function(blocks) {
  out_lines <- unlist(
    lapply(blocks, function(b) b$lines),
    use.names = FALSE
  )

  paste(out_lines, collapse = "\n")
}

Design rule:
The assembler restores original block order and does not interpret
metadata or modify syntax.


WHY THIS ARCHITECTURE MATTERS

This separation of responsibilities:

- avoids duplicated logic for groups, levels, and other contexts
- guarantees idempotence of transformations
- allows new structural contexts to be added without rewriting
  existing code

For example, adding support for multilevel SEM only requires a new
parser that recognizes level: blocks.


RELATIONSHIP TO TIME AND OTHER DIMENSIONS

Not all statistical dimensions correspond to blocks.

Time in longitudinal SEM is typically encoded in variable names:

x_t1 =~ x1_t1 + x2_t1
x_t2 =~ x1_t2 + x2_t2

In these cases:
- no time parser is required
- time is handled by orchestrators and labeling rules
- block mapping remains unchanged


SUMMARY OF RESPONSIBILITIES

Parser:
- Identify blocks and attach metadata

Mapper:
- Iterate over blocks

Callback:
- Modify block contents

Assembler:
- Recombine blocks


EXTENSION GUIDANCE

Can a parser do many different things?
Yes, but with an important constraint.

Parsers may identify blocks based on any syntactic criterion, such as:
- group: headers
- level: headers
- subsets of syntax matching a pattern

However, parsers should identify structural contexts, not analysis
targets.

Use parsers to define where transformations apply, not what
transformations are performed.


OPERATING ACROSS MULTIPLE FACTORS

This architecture naturally supports models with:
- multiple time-specific factors
- multiple within-person targets (e.g., ratings of several friends)
- method or informant factors

Constraints across factors are implemented via:
- callback logic that inspects factor names
- label-engine rules that equate parameters
- orchestrators that coordinate transformations

No new parsers are required for cross-factor constraints.


HOW TO ADD A NEW PARSER

Step 1: Decide what defines a block.
Determine:
- what line marks the start of a block
- where the block ends
- what metadata should be attached

Examples:
- group: for multi-group SEM
- level: for multilevel SEM
- no marker for single-block models

Step 2: Write a function that returns blocks.
A parser must return a list of blocks, where each block is a list with:

list(
  lines   = character(),
  indices = list()
)

Example:

parse_single_block <- function(model_string) {
  lines <- strsplit(model_string, "\n", fixed = TRUE)[[1]]

  list(
    list(
      lines   = lines,
      indices = list()
    )
  )
}

Step 3: Do not modify syntax in the parser.
Parsers must not:
- add or remove model lines
- impose constraints
- generate new parameters

They only locate blocks and attach metadata.

Step 4: Use the parser with .map_blocks.

.map_blocks(
  model_string = ms,
  parser       = parse_single_block,
  FUN          = my_callback
)

No changes to .map_blocks or existing setters are required.


GUIDING PRINCIPLE

Use parsers to define structural contexts and callbacks to express
statistical intent.

Maintaining this separation allows the package to grow without
becoming brittle or overcomplicated.







