# ghc-gc-tune

Graph the performance of a Haskell program as GC settings change.

Originally written by [Don Stewart](https://donsbot.wordpress.com/2010/07/05/ghc-gc-tune-tuning-haskell-gc-settings-for-fun-and-profit/).

## What it does

`ghc-gc-tune` runs your compiled Haskell program with different combinations of GC flags (`-A` allocation area size, `-H` suggested heap size) and generates 3D surface plots showing how performance varies across the tuning space.

This lets you automatically find the optimal GC settings for your program.

## Requirements

- GHC (for compiling your program with RTS support)
- [gnuplot](http://www.gnuplot.info/) (for rendering graphs)

## Installation

```
cabal install ghc-gc-tune
```

Or build from source:

```
cabal build
```

## Usage

```
ghc -O2 --make your-program.hs
ghc-gc-tune -t svg your-program [program args...]
```

### Output formats

- `pdf` - PDF via pdfcairo
- `png` - PNG
- `pngcairo` - PNG via cairo backend
- `svg` - SVG
- `html` - HTML canvas
- (no flag) - interactive gnuplot window

### Options

```
ghc-gc-tune [OPTS...] ./path/to/executable [PROG_ARGS...] [--RTS +RTS GHC_RTS_OPTIONS...]

  -h       --help         Print this help message.
  -t TYPE  --type=TYPE    Output type: pdf, png, svg, pngcairo, html.
           --Amin=N       Minimum allocation size to sample (default 16K).
           --Amax=N       Maximum allocation size to sample (default 512M).
           --Hmin=N       Minimum initial heap size to sample (default 1M).
           --Hmax=N       Maximum initial heap size to sample (default 1G).
           --Mmax=N       Maximal heap size for process (default unlimited).
  -s       --Time         Create graph of time (default).
  -p       --Peak         Create graph of peak memory allocation.
  -r       --Residency    Create graph of maximum resident memory.
  -c       --Config       Create config file with personal default options.
```

### Configuration

Default options can be overridden in `~/.ghc-gc-tune.config`. Use `-c` to generate a template config file. Sizes support `K`, `M`, `G` suffixes.

## Compatibility

Supports GHC 9.2+ (including the changed RTS statistics field names in GHC 9.x). Also retains backward compatibility with GHC 8.x field names.
