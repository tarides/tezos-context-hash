# Tezos context hashes

:::danger
***Status**: Stable*
***Version**: 2021-03-03*
***Contact**: thomas@tarides.com*
:::

This document provides a specification for computing the pre-image hashes of all kind of objects present in the Tezos context. Tezos then uses 32-bytes [BLAKE2B-256](https://www.blake2.net/) to compute hashes from pre-image hashes. Moreover, hashes are displayed using a base 58 representation and are prefixed by `Co`. For instance, `CoVGWKM7Ufu6dk74CEQz3MgffhUPFyeaMCD6eS3Q8o7mDis8n1Vi` is a valid context hash for Tezos.

:::info
**Notations:** 

- `blake2b(s)` is the BLAKE2B-256 hash of the string `s`.
- `prehash(v)` is the pre-hash image of the object `v`.
- `hash(v) = blake2b(prehash(v))` is the cryptographic hash of the object `v`.
- `len(s)` is the length of the string `s` (in bytes).
:::

This documents uses a graphical notation to represent pre-hash image:

- Let `s` a pre-image hash and `n = len(s)` be its size in byte. This is represented as:
  |  n  |
  |:---:|
  |  s  |
- Let `s` and `t` be 2 pre-image hashes and `n = len(s)` and `m = len(t)` be their respective sizes (in bytes). The concatenation`s` and `t` is represented as:
  |  n  |  m  |
  |:---:|:---:|
  |  s  |  t  |

## Integers

The Tezos context has two modes for getting the pre-image hashes of integers:
- Fixed-length mode: the integer is encoded using 8 bytes, with big-endian conventions.
- Variable length mode: the integer is encoded in a variable length string, using [unsigned LEB128 encoding](https://en.wikipedia.org/wiki/LEB128). This encoding has the following good property: positive integers smaller than 128 have the same binary representation using both variable-size and fixed-size encoding.


:::info
**Notations:**

- we will denote the encoding of integers with **8** when fixed length encoding is used, and **(LEB128)** when the variable length encoding is used, and the size is not known in advance. 
- We will write `\x` instead of the binary representation of the integer `x`, e.g. `\32` instead of `\000\000\000\000\000\000\000\032` for fixed-length encoding or `\032` for variable-length encoding.
:::

:::warning 
**Example: hashing the integer `1298532`.**
</br>

- In fixed-length mode:
  |                8                |
  |:-------------------------------:|
  | `\000\000\000\000\000\019\208\100` |
* In variable-length mode:
  |  (LEB128)   |
  |:-----------:|
  | `\228\160\079` |
:::

## Contents

Contents are raw sequences of bytes. Getting their pre-image hash is done by concatenating their length encoded on 8 bytes, along with their value:

|     8     | `len(v)` |
|:---------:|:------:|
| `\len(v)` |  `v`   | 

:::warning 
**Example: hashing the contents `"delphi_007"`.**
</br>
|   8   |      10      |
|:-----:|:------------:|
| `\000\000\000\000\000\000\000\010` | `delphi_007` |
:::

## Commits

Commits are immutable objects that contain metadata about changes done to the context, as well as pointers to previous commits and to the current Merkle trees root. These objects allow Tezos to track the history of all the changes done to the context.

**Commits metadata are triplet: *date $\times$ author $\times$ message*.** *Date* values are expressed in seconds and are stored as an `int64`. *Author* is usually the string `"Tezos"` but could change in the future. *Message* is a string containing information about the corresponding Tezos block such as its level, fit, priority and number of operations.  

A commit metadata is encoded as follows:

|    8    |       8        | `len(author)` |        8        | `len(message)` |
|:-------:|:--------------:|:-------------:|:---------------:|:--------------:|
| `\date` | `\len(author)` |   `author`    | `\len(message)` |   `message`    |

:::warning
**Example: hashing the commit metdata with date 1612521119 and message `"msg"`.**
</br>
|                 8                  |                 8                  |    5    |                 8                 |   3   |
|:----------------------------------:|:----------------------------------:|:-------:|:---------------------------------:|:-----:|
| `\000\000\000\000\039\029\030\159` | `\000\000\000\000\000\000\000\005` | `Tezos` | `\000\000\000\000\000\000\000\03` | `msg` |
:::

**Commits are triplet: *tree $\times$ parents $\times$ metadata*.** *Tree* is a hash pointing to the Merkle tree root. *Parents* are a list of commit hashes which are immediate predecessors of the current commit. A commit with $k$ parent hashes $p_1$, ..., $p_k$ is encoded as follows:

|   8   |   32   |  8  |   8   |  32   | ... |   8   |  32   |          m          |
|:-----:|:------:|:---:|:-----:|:-----:|:---:|:-----:|:-----:|:-------------------:|
| `\32` | `tree` | `\k` | `\32` | $p_1$ | ... | `\32` | $p_k$ | `prehash(metadata)` |

where *m = len(prehash(metadata))*.

:::warning
**Example: hashing the commit with tree hash `<tree>`, one parent `<parent>` and the metadata with pre-image hash`<m>`.**
</br>

|                 8                  |    32    |               8                |                 8                  |     32     | `len(m)` |
|:----------------------------------:|:--------:|:------------------------------:|:----------------------------------:|:----------:|:------:|
| `\000\000\000\000\000\000\000\032` | `<tree>` | `\000\000\000\000\000\000\001` | `\000\000\000\000\000\000\000\032` | `<parent>` | `<m>`  |

:::

## Trees 

The Tezos context contains [Merkle trees](https://en.wikipedia.org/wiki/Merkle_tree). Contents hashing has already been explained [in a previous section](#Contents). This section exposes the pre-image hash calculation for internal nodes of the tree. In Tezos, trees nodes are equivalent to a list of entries, pointing to the immediate children of the node. 

**A tree entry is a triplet: *name* $\times$ *kind* $\times$ *hash*.** *Names* are strings, similar to file or directory names. *Kind* distinguishes between tree and contents and *hash* points to other data present in the context. Its encoding will depend on the internal representation of nodes.

The complexity of verifying (and computing) the hash of tree nodes depends on the number of its entries. To reduce that cost on large nodes, Tezos distinguishes between tree nodes holding at most 256 entries and tree nodes with more than 256 ones. The small nodes are represented as a list of entries and have a linear complexity, while large nodes are using an optimised representation -- similar to inode tables implemented by filesystems -- to handle large directories with a logarithmic complexity. 

Until now in Tezos (i.e. Edo), all the directories have less than 256 entries and so do not use this optimised representation. This might change with sappling.

:::info
**Tree nodes representations**

The pre-image hashes of tree nodes depends on the number `n` of entries present in that tree node.
- when `n <= 256`, Tezos encodes the entries as a list of *bindings*. See bellow for encoding of [nodes](#Nodes).
- when `n > 256`, Tezos encode the entries as an optimised tree of *inodes*. See bellow for encoding of [inodes](#Inodes).
:::

### Nodes

Directories with less than 256 entries are encoded as a flat list of entries.

As already described above, **a tree entry is a triplet: *name $\times$ kind $\times$ hash*.** An entry is encoded as follow:

|   8    |  `(LEB128)`  | `len(name)` |   8   |   32   |
|:------:|:------------:|:-----------:|:-----:|:------:|
| `kind` | `\len(name)` |   `name`    | `\32` | `hash` |

`kind` is either:
- the string `"\255\000\000\000\000\000\000\000"` if the entry is pointing to a contents.
- the string `"\000\000\000\000\000\000\000\000"` if the entry is pointing to a node.

:::warning
**Example: hashing an entry whose name is `"protocol"`, pointing to some contents with hash `<hash>`.**
</br>

|                 8                  |   1    |     8      |                 8                  |    32    |
|:----------------------------------:|:------:|:----------:|:----------------------------------:|:--------:|
| `\255\000\000\000\000\000\000\000` | `\008` | `protocol` | `\000\000\000\000\000\000\000\032` | `<hash>` |
:::



**A node is a list of at most 256 entries, sorted by lexicographic order of names.** A node with $n$ entries,  $e_1$, $e_2$, ... $e_n$ is encoded as follow when $n \le 256$:

|  8   |     $n_1$      | ... |     $n_n$      |
|:----:|:--------------:|:---:|:--------------:|
| `\n` | `prehash(e_1)` | ... | `prehash(e_n)` |

where *$n_i$ = len(prehash($e_i$))* and *name($e_1$) $\lt$ ... $\lt$ name($e_n$)*. Hence all names must be different.


### Inodes

Because of storage optimisations implemented in Tezos, the pre-image hash of directories with more than 256 entries is computed in a different way, and requires some transformations prior to its computation. In fact,  inodes are themselves represented as trees. There are two kinds of inode objects: values (leafs) and trees.

#### Inode values

An inode value contains a flat list of tree entries. 

As already described, **a tree entry is a triplet: *name $\times$ kind $\times$ hash*.** The encoding for inode entries is more compact than for node entries. For inodes, entries are encoded as follows:

|  `(LEB128)`  | `len(name)` |   1    |   32   |
|:------------:|:-----------:|:------:|:------:|
| `\len(name)` |   `name`    | `kind` | `hash` | 

where *kind* is `\000` for nodes, and `\001` for contents.

:::warning
**Example: hashing an entry whose name is `"protocol"` and pointing to some contents with hash `<hash>`.**
</br>

|   1    |     8      |   1    |    32    |
|:------:|:----------:|:------:|:--------:|
| `\008` | `protocol` | `\001` | `<hash>` |
:::


**An inode value is a list of at most 32 entries, sorted by lexicographic order of names.** A value with $n$ entries $e_1$, $e_2$, ... $e_n$ is encoded as follows when $n \leq 32$:

|   1    |  1   |     $n_1$      | ... |     $n_k$      |
|:------:|:----:|:--------------:|:---:|:--------------:|
| `\000` | `\n` | `prehash(e_1)` | ... | `prehash(e_k)` |

where *$n_i$ = len(prehash($e_i$))* and *name($e_1$) $\le$ ... $\le$ name($e_n$)*. Hence all names must be different.

#### Inode trees

Inode trees contain a list of inode pointers

**An inode pointer is a pair: *index $\times$ hash*.** *index* is always strictly less than 32. A pointer is encoded as follows:

|    1    |   32   |
|:-------:|:------:|
| `index` | `hash` | 

**An inode tree is a triplet: *depth $\times$ len(children) $\times$ pointers*.** The number of pointers is always less than 32. $pointers$ is a sparse list of pointers: every pointer in the list has a distinct index and the list is ordered by increasing indices, but some indices might be missing. Moreover, `len(children)` aggregates the total number of entries that are reachable via the pointers. This information can for instance be used to decide whenever an inode value has to be converted into a inode tree (or the reverse) when a new entry is added (or removed) from the tree.

An inode tree with `k` pointers $p_1$, $p_2$, ... $p_k$ is encoded as follow:

|   1    | `(LEB128)` |   `(LEB128)`    |  1   |  33   | ..  |  33   |
|:------:|:----------:|:---------------:|:----:|:-----:|:---:|:-----:|
| `\001` |  `depth`   | `len(children)` | `\k` | $s_1$ | ... | $s_k$ |

where *$s_i$ = prehash($p_i$)*.

:::warning
**Example: hashing an inode tree of depth `0`, length `50000` and a list of pointers with pre-image hashes $s_1$, ... $s_{32}$.**
</br>

|   1    |     1      |     3    |  1   |  33   | ..  |  33   |
|:------:|:----------:|:------:|:--------:|:-----:|:---:|:-----:|
| `\001` | `\000` | `\160\194\030` | `\032` |$s_1$ | ... | $s_{32}$ |
:::

#### Constructing the internal tree

So far we have described only how to encode inodes but not how to construct them. This section explain how to do this and turn a list of entries into tree-like structure of inode trees and values.

The goal is thus to partition a list of entries of size greater than 256 into disjoint groups of entries of at most 32 elements, corresponding to inode values described above; and to create intermediate inode trees to efficiently dispatch names to the right leaf value using inode pointers.

##### Index function

The crux of the algorithm is to compute indices. Given an entry with name `n`, its index at depth `d` is defined as:

```clike
index(d, n) =
    abs (ocaml_hash(d, n)) mod 32
```

where `ocaml_hash` is a custom, non-crypto hash function defined by the [the OCaml's runtime](https://github.com/ocaml/ocaml/blob/trunk/runtime/hash.c#L145-L175) and based on [MurmurHash3](https://github.com/aappleby/smhasher/wiki/MurmurHash3) and defined as follows:

```clike=
mix(h: <uint32>, w: <uint32>) {
    w *= 0xcc9e2d51;
    w = w << 15 | w >> 17;
    w *= 0x1b873593;
    h ^= w;
    h = h << 13 | h >> 19;
    h = h * 5 + 0xe6546b64;
}

uint32 ocaml_hash(seed: <uint32>, s: <string>) {
    uint32 h = seed;
    for (i = 0; i < len(s); i = i + 4) {
        uint32 w;
        if big_endian_machine     
            w = rev(s[i:i+4])
        else
            w = s[i:i+4];
        mix(h, w);
    }
    // At this point there might be up to 3 bytes left to read.
    if i <> len {
        // Bytes that are out of range should be set to \000.
        uint32 w = rev(s[i:i+4]);
        mix(h, w);   
    }    
    h ^= (uint32) len(s);
    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;
    return h & 0x3FFFFFFFU;
}
```

##### Inode decomposition

First, let us define a few notations. Let $\textbf{Value}(X)$ denotes the [inode value](#Inode-values) with entries $X$. Let $\textbf{Empty}$ be the empty inode value, i.e. $\textbf{Value}(\{\})$. Let $\textbf{Pointer}(i \times h)$ be the [inode pointer](#Inode-tree) with index $i$ and hash $h$. And let $\textbf{Tree} (d \times n \times P)$ the [inode tree](#Inode-tree) with depth $d$, number of children $n$ and containing the list of pointers $P$.

Let $X$ be a set of inode entries $e_1$, $e_2$, ... $e_n$.  Let's define the subset $X_{d,j}$ of $X$ as the set of elements with the same index $j$ at depth $d$:
$$
X_{d,j} = \{ e_i \in X ~|~ \textbf{index} (d, \text{name}(e_i)) = j \}
$$

The following function partitions recursively a list of entries $X$ into a collection of inode trees and values, starting at depth $d$:
$$
\begin{align}
&\textbf{partition }(d, X) = \\
&\quad \text{if }len (X) = 0 \text{ then } \textbf{Empty}\\
&\quad \text{elif }len (X) \le 32 \text{ then } \textbf{Value}(X)\\
&\quad \text{else }\\
&\qquad\quad \text{ for }j = 0..31\text{ do }\\
&\qquad\qquad t_j = \textbf{partition} (d+1, \ X_{d,j})\\
&\qquad\qquad p_j = \textbf{Pointer}(j \times hash(t_j))\\
&\qquad\quad \text{ done }\\
&\qquad\quad P = \{ p_i \ | \ t_i \neq \textbf{Empty} \}_{i \leq 31}\\
&\qquad\quad\textbf{Tree} (depth \times len (X) \times P)\\
\end{align}
$$

$\textbf{Empty}$ will never appear in the Tezos context directly, as the storage layer always automatically remove empty directories. But it is an useful concept to manipulate sparse lists of pointers.

The pre-image hash of a tree node with $n$ entries $X$, when $n > 256$, is the pre-hash image of the result of $\textbf{partition}(0, X)$:

- The pre-image hash of $\textbf{Value}(X)$ is defined in section about [inode values](#Inode-values).
- The pre-image hash of $\textbf{Tree}(d \times l \times P)$ is defined in section about [inode trees](#Inode-trees).

##### Inode search

$$
\begin{align}
&\textbf{find }(n, \textbf{Value} (X)) =\\
&\qquad \text{ if } e \in X \ | \ name(e) = n  \text{ then } \textbf{Some }e \\
&\qquad \text{ else }\textbf{None}\\
&\textbf{find }(n, \textbf{Tree } (depth\times \_ \times \bigcup p_{i})) = \\
&\qquad j = \textbf{index}(depth, \ n)\\
&\qquad t_j = \textbf{read} (p_j) \\
&\qquad \textbf{find }(n, t_j)\\
\end{align}
$$

##### Inode add

$$
\begin{align}
&\textbf{add }(d, e, \textbf{Value} (X)) =\\
&\qquad \text{ if } len(X) \lt 32 \text{ then }\textbf{Value}(X \cup \{e\}) \\
&\qquad \text{ else }\textbf{partition}(d, X \cup \{ e \})\\
&\textbf{add }(d, e, \textbf{Tree } (\_ \times len \times \bigcup p_{i})) = \\
&\qquad j = \textbf{index}(d, \ name(e))\\
&\qquad t_j = \textbf{read} (p_j) \\
&\qquad t'_j = \textbf{add }(d+1, e, t_j)\\
&\qquad p'_j = \textbf{Pointer }(j \times hash(t'_j))\\
&\qquad P' = \bigcup p_{i} \cup \{ p'_j \} - \{ p_j \}\\
&\qquad \textbf{Tree } (d \times (len+1) \times P') 
\end{align}
$$


<!---
__wip__

> @ioana : values here are node_pointer, right?

The type of the intermediate nodes:
```clike
type ptr = { index : int[0..31]; target : t }
type tree = { depth : int; length : int; entries : ptr list }
type t = Values of (step * value) list | Tree of tree
```
where `entries` is a list ordered by the `index` of each entry.

We construct them from a list of `(step, value)`.

```clike
add depth (t : tree) (s : step) (v : value) =
  match t with 
	| Values vs -> if len(vs) <= 32 
                       then Values ((s,v) :: vs) 
			else 
			begin
				empty_tree <- Tree {length =0; depth; []};
				vs <- (s,v) :: vs;
				List.fold_left (fun t (s,v) -> add depth t s v) empty_tree vs
                       end
        | Tree t -> begin
                    index <- index ~depth s;
                    length <- length + 1;
                    try
                        {index; target} <- find t.entries index;
                        t <- add (depth+1) target s v;
                        entries <- replace t.entries {index; t};
                        Tree {t with entries; length}
                    with Not_found ->
                      begin
                          entry <- {index; Values [(s,v)]};
                          entries <- add_and_sort t.entries entry;
                          Tree {t with entries; length}
                      end
                    end
                   
```
--->

<!---
**Examples.**
Note: the following examples are not, in fact, unstable, as they contain less than 256 children. For the sake of keeping these short though, we show how their pre-hash would be computed if they were unstable.
:::

- for `v={"Tree":{"depth":0,"length":3,"entries":[{"index":1,"hash":"9c881113b87ff6edb24e2de8241b5cfa01325048"}]}}` then `pre_hash(v)=\001\000\003\001\001\156\136\017\019\184\127\246\237\178N-\232$\027\\\250\0012PH `

| `prehash(Tree)` | `prehash(v.depth)` | `prehash(v.length)` | `prehash(len([..]))`| `prehash(index)` | `prehash(hash) = hash`
| -------- | -------- | -------- |-------- |-------- |-------- |-------- |
| `\001`     | `\000`     | `\003`     | `\001` | `\001` | `\156\136\017\019\184\127\246\237\178N-\232$\027\\\250\0012PH`
--->

<!---
- for `v={"Values":[]}` the variant code is `\000` and `prehash([]) =\000` as `len([])=0`;
- for `v = {"Values":[["c",{"contents":"5d62a997286a031d4640821315adc8911645945c056642edff0e49238a0909909bf7329e1162bf7f059887e38bc1ceafd02c417dd59463a1db0bf591b7c0f118"}]]}` we have 
`pre_hash(v) = pre_hash(Values)||pre_hash(len([..]))||pre_hash("c")||pre_hash(Contents)||pre_hash(hash) =
(\000) || (\001)||(\001c) || (\001) || 
]b\169\151(j\003\029F@\130\019\021\173\200\145\022E\148\\\005fB\237\255\014I#\138\t\t\144\155\2472\158\017b\191\127\005\152\135\227\139\193\206\175\208,A}\213\148c\161\219\011\245\145\183\192\241\024`  
where `pre_hash(hash)` is just the ascii code for the hex ``"5d62...18"``;
--->

