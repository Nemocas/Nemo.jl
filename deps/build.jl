using Libdl

using BinaryProvider

# Parse some basic command-line arguments
const verbose = "--verbose" in ARGS

issource_build = "NEMO_SOURCE_BUILD" in keys(ENV) && ENV["NEMO_SOURCE_BUILD"] == "1"

const prefixpath = joinpath(@__DIR__, "usr")

const wdir = joinpath(@__DIR__)

if !issource_build
  # Dependencies that must be installed before this package can be built
  dependencies = [
    # This has to be in sync with the corresponding commit in the source build below (for flint, arb, antic)
    "https://github.com/JuliaPackaging/Yggdrasil/releases/download/GMP-v6.1.2-1/build_GMP.v6.1.2.jl",
    "https://github.com/JuliaPackaging/Yggdrasil/releases/download/MPFR-v4.0.2-1/build_MPFR.v4.0.2.jl",
    "https://github.com/thofma/Flint2Builder/releases/download/d46056/build_libflint.v0.0.0-d46056d45c6429f58ec63cf3c2e59b00f8431479.jl",
    "https://github.com/thofma/ArbBuilder/releases/download/6c3738-v2/build_libarb.v0.0.0-6c3738555d00b8b8b24a1f5e0065ef787432513c.jl",
    "https://github.com/thofma/AnticBuilder/releases/download/364f97-v2/build_libantic.v0.0.0-364f97edd9b6af537787113cf910f16d7bbc48a3.jl"
   ]

  const prefix = Prefix(get([a for a in ARGS if a != "--verbose"], 1, joinpath(@__DIR__, "usr")))

  products = []

  for url in dependencies
      build_file = joinpath(@__DIR__, basename(url))
      if !isfile(build_file)
          download(url, build_file)
      end
  end

  # Execute the build scripts for the dependencies in an isolated module to avoid overwriting
  # any variables/constants here
  for url in dependencies
      build_file = joinpath(@__DIR__, basename(url))
      m = @eval module $(gensym()); include($build_file); end
      append!(products, m.products)
  end

  filenames = ["libgmp.la", "libgmpxx.la", "libmpfr.la"]
  for filename in filenames
    fpath = joinpath(prefixpath, "lib", filename)
    txt = read(fpath, String)
    open(fpath, "w") do f
      write(f, replace(txt, "/workspace/destdir" => prefixpath))
    end
  end

else
  println("Doing a source build for C dependencies...")
  if "NEMO_BUILD_THREADS" in keys(ENV)
     build_threads = ENV["NEMO_BUILD_THREADS"]
     println("Using $build_threads threads for building as specified by NEMO_BUILD_THREADS.")
  else
     build_threads = Sys.CPU_THREADS
     println("Using $build_threads threads (detected that many CPU threads).")
  end

  @show M4_VERSION = "1.4.17"
  @show YASM_VERSION = "1.3.0"
  @show MPIR_VERSION = "3.0.0-90740d8fdf03b941b55723b449831c52fd7f51ca"
  @show MPFR_VERSION = "4.0.0"
  @show FLINT_VERSION = "d46056d45c6429f58ec63cf3c2e59b00f8431479"
  @show ARB_VERSION = "6c3738555d00b8b8b24a1f5e0065ef787432513c"
  @show ANTIC_VERSION = "364f97edd9b6af537787113cf910f16d7bbc48a3"

  if Sys.iswindows()
    error("Source build not available on Windows")
  end

  println("Removing old binaries ...")

  rm(prefixpath, force = true, recursive = true)
  mkdir(prefixpath)
  mkdir(joinpath(prefixpath, "lib"))

  if Sys.isapple() && !("CC" in keys(ENV))
   ENV["CC"] = "clang"
   ENV["CXX"] = "clang++"
  end

  LDFLAGS = "-Wl,-rpath,$prefixpath/lib -Wl,-rpath,\$\$ORIGIN/../share/julia/site/v$(VERSION.major).$(VERSION.minor)/Nemo/local/lib"
  DLCFLAGS = "-fPIC -fno-common"

  cd(wdir)

  try
    run(`m4 --version`)
  catch
    println("Building m4 ... ")
    M4_FILE = "m4-" * M4_VERSION * ".tar.bz2"
    download("http://ftp.gnu.org/gnu/m4/$M4_FILE", joinpath(wdir, "$M4_FILE"))

      run(`tar -xvf $M4_FILE`)
      run(`rm $M4_FILE`)
      cd(joinpath("$wdir", "m4-$M4_VERSION"))
      run(`./configure --prefix=$prefixpath`)
      run(`make`)
      run(`make install`)
      println("DONE")
   end

  cd(wdir)

  # install yasm

  if !ispath(joinpath(wdir, "yasm-$YASM_VERSION"))
     println("Building yasm ... ")
     YASM_FILE = "yasm-" * YASM_VERSION * ".tar.gz"
     download("https://github.com/yasm/yasm/archive/v$(YASM_VERSION).tar.gz", YASM_FILE)
     run(`tar -xvf $YASM_FILE`)
     run(`rm $YASM_FILE`)
     cd(joinpath("$wdir","yasm-$YASM_VERSION"))
     run(`./autogen.sh`)
     run(`./configure`)
     run(`make`)
     println("DONE")
  end

  cd(wdir)

  # install GMP/MPIR

  MPIR_FILE = "mpir-" * MPIR_VERSION * ".tar.bz2"

  if !ispath(joinpath(wdir, "mpir-$MPIR_VERSION"))
    println("Downloading MPIR sources ... ")
    download("http://nemocas.org/binaries/$MPIR_FILE", joinpath(wdir, MPIR_FILE))
    println("DONE")
  end

  println("Building MPIR ... ")
  if isfile(joinpath(wdir, MPIR_FILE))
     run(`tar -xvf $MPIR_FILE`)
     run(`rm $MPIR_FILE`)
  end
  cd("$wdir/mpir-$MPIR_VERSION")
  try
     run(`m4 --version`)
     run(`./configure --with-yasm=$wdir/yasm-$YASM_VERSION/yasm --prefix=$prefixpath --enable-gmpcompat --disable-static --enable-shared`)
  catch
     run(`./configure --with-yasm=$wdir/yasm-$YASM_VERSION/yasm --prefix=$prefixpath M4=$prefixpath/bin/m4 --enable-gmpcompat --disable-static --enable-shared`)
  end
  run(`make -j$build_threads`)
  run(`make install`)
  cd(wdir)
  run(`rm -rf bin`)
  println("DONE")

  cd(wdir)

  # install MPFR

  MPFR_FILE = "mpfr-" * MPFR_VERSION * ".tar.bz2"

  if !ispath(joinpath(wdir, "mpfr-$MPFR_VERSION"))
     println("Downloading MPFR sources ... ")
     download("http://ftp.vim.org/ftp/gnu/mpfr/$MPFR_FILE", joinpath(wdir, MPFR_FILE))

     println("DONE")
  end

  println("Building MPFR ... ")
  if isfile(joinpath(wdir, MPFR_FILE))
    run(`tar -xvf $MPFR_FILE`)
    run(`rm $MPFR_FILE`)
  end
  cd("$wdir/mpfr-$MPFR_VERSION")
  withenv("LD_LIBRARY_PATH"=>"$prefixpath/lib", "LDFLAGS"=>LDFLAGS) do
    run(`./configure --prefix=$prefixpath --with-gmp=$prefixpath --disable-static --enable-shared`)
    run(`make -j$build_threads`)
    run(`make install`)
  end
  println("DONE")

  cd(wdir)

  # INSTALL FLINT

  try
    println("Cloning flint2 ... ")
    run(`git clone https://github.com/wbhart/flint2.git`)
    cd(joinpath("$wdir", "flint2"))
    run(`git checkout $FLINT_VERSION`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "flint2"))
      open(`patch -R --forward -d flint2 -r -`, "r", open("../deps-PIE-ftbfs.patch"))
      cd(joinpath("$wdir", "flint2"))
      run(`git fetch`)
      run(`git checkout $FLINT_VERSION`)
      cd(wdir)
    end
  end
  open(`patch --forward -d flint2 -r -`, "r", open("../deps-PIE-ftbfs.patch"))
  println("DONE")

  println("Building flint ... ")
  cd(joinpath("$wdir", "flint2"))
  withenv("LD_LIBRARY_PATH"=>"$prefixpath/lib", "LDFLAGS"=>LDFLAGS) do
    run(`./configure --prefix=$prefixpath --disable-static --enable-shared --with-mpir=$prefixpath --with-mpfr=$prefixpath`)
    run(`make -j$build_threads`)
    run(`make install`)
  end

  println("DONE")

  cd(wdir)

  # INSTALL ARB

  println("Cloning arb ... ")
  try
    run(`git clone https://github.com/fredrik-johansson/arb.git`)
    cd(joinpath("$wdir", "arb"))
    run(`git checkout $ARB_VERSION`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "arb"))
      #open(`patch -R --forward -d arb -r -`, "r", open("../deps-PIE-ftbfs.patch"))
      cd(joinpath("$wdir", "arb"))
      run(`git fetch`)
      run(`git checkout $ARB_VERSION`)
      cd(wdir)
    end
  end
  println("DONE")

  println("Building arb ... ")
  cd(joinpath("$wdir", "arb"))
  withenv("LD_LIBRARY_PATH"=>"$prefixpath/lib", "LDFLAGS"=>LDFLAGS) do
    run(`./configure --prefix=$prefixpath --disable-static --enable-shared --with-mpir=$prefixpath --with-mpfr=$prefixpath --with-flint=$prefixpath`)
    run(`make -j$build_threads`)
    run(`make install`)
  end
  println("DONE")

  cd(wdir)

  # INSTALL ANTIC

  println("Cloning antic ... ")
  try
    run(`git clone https://github.com/wbhart/antic.git`)
    cd(joinpath("$wdir", "antic"))
    run(`git checkout $ANTIC_VERSION`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "antic"))
      #open(`patch -R --forward -d antic -r -`, "r", open("../deps-PIE-ftbfs.patch"))
      cd(joinpath("$wdir", "antic"))
      run(`git fetch`)
      run(`git checkout $ANTIC_VERSION`)
      cd(wdir)
    end
  end
  println("DONE")

  println("Building antic ... ")
  cd(joinpath("$wdir", "antic"))
  withenv("LD_LIBRARY_PATH"=>"$prefixpath/lib", "LDFLAGS"=>LDFLAGS) do
    run(`./configure --prefix=$prefixpath --disable-static --enable-shared --with-mpir=$prefixpath --with-mpfr=$prefixpath --with-flint=$prefixpath`)
    run(`make -j$build_threads`)
    run(`make install`)
  end
  println("DONE")
  cd(wdir)
end

push!(Libdl.DL_LOAD_PATH, joinpath(prefixpath, "lib"), joinpath(prefixpath, "bin"))

