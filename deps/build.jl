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
    "https://github.com/JuliaMath/GMPBuilder/releases/download/v6.1.2-2/build_GMP.v6.1.2.jl",
    "https://github.com/JuliaMath/MPFRBuilder/releases/download/v4.0.1-3/build_MPFR.v4.0.1.jl",
    "https://github.com/thofma/Flint2Builder/releases/download/reentrant2/build_libflint.v0.0.0-5451b53703a529ff76123b7418fe2d624e122db6.jl",
    "https://github.com/thofma/ArbBuilder/releases/download/56ce68/build_libarb.v0.0.0-56ce687ea1ff9a279dc3c8d20f31a4dd09bae6d1.jl",
    "https://github.com/thofma/AnticBuilder/releases/download/e2788/build_libantic.v0.0.0-e2788d9be58dc9850b7557689a6eb05f5cdb746b.jl"
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

  @show M4_VERSION = "1.4.17"
  @show YASM_VERSION = "1.3.0"
  @show MPIR_VERSION = "3.0.0-90740d8fdf03b941b55723b449831c52fd7f51ca"
  @show MPFR_VERSION = "4.0.0"
  @show ANTIC_VERSION = "e2788d9be58dc9850b7557689a6eb05f5cdb746b"
  @show FLINT_VERSION = "5451b53703a529ff76123b7418fe2d624e122db6"
  @show ARB_VERSION = "56ce687ea1ff9a279dc3c8d20f31a4dd09bae6d1"

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
     download("http://www.tortall.net/projects/yasm/releases/$YASM_FILE", YASM_FILE)
     run(`tar -xvf $YASM_FILE`)
     run(`rm $YASM_FILE`)
     cd(joinpath("$wdir","yasm-$YASM_VERSION"))
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
  run(`make -j4`)
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
    run(`make -j4`)
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
    run(`make -j4`)
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
    run(`make -j4`)
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
    run(`make -j4`)
    run(`make install`)
  end
  println("DONE")
  cd(wdir)
end

push!(Libdl.DL_LOAD_PATH, joinpath(prefixpath, "lib"), joinpath(prefixpath, "bin"))
