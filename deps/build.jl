using Libdl

oldwdir = pwd()

@show M4_VERSION = "1.4.17"
@show YASM_VERSION = "1.3.0"
@show MPIR_VERSION = "3.0.0-90740d8fdf03b941b55723b449831c52fd7f51ca"
@show MPFR_VERSION = "4.0.0"
@show ANTIC_VERSION = "96b37f6242526f95f68f1f15c925db5a4a19a21c"
@show FLINT_VERSION = "b44e31c4b456653a54d046b094491039d0cde612"
@show ARB_VERSION = "987e7a1395d7dd608139b6ac07ba889cc4fadbd9"

pkgdir = dirname(dirname(@__FILE__))
wdir = joinpath(pkgdir, "deps")
vdir = joinpath(pkgdir, "local")

if "NEMO_MAKE_CLEAN" in keys(ENV) && ENV["NEMO_MAKE_CLEAN"] == "1"
  print("
===============================================================================
=
=  NEMO_MAKE_CLEAN = 1
=  Removing old sources and builds
=
================================================================================\n")

  rm(joinpath(wdir, "flint2"), force = true, recursive = true)
  rm(joinpath(wdir, "arb"), force = true, recursive = true)
  rm(joinpath(wdir, "antic"), force = true, recursive = true)
  rm(joinpath(wdir, "mpfr-4.0.0"), force = true, recursive = true)
  rm(joinpath(wdir, "mpir-3.0.0"), force = true, recursive = true)
  rm(joinpath(wdir, "yasm-1.3.0"), force = true, recursive = true)
  rm(vdir, force = true, recursive = true)
end

if Sys.isapple() && !("CC" in keys(ENV))
   ENV["CC"] = "clang"
   ENV["CXX"] = "clang++"
end

if !ispath(vdir)

    mkdir(vdir)

    if !ispath(joinpath(vdir, "lib"))
        mkdir(joinpath(vdir, "lib"))
    end
else
    println("Deleting old $vdir")
    rm(vdir, force=true, recursive=true)
    mkdir(vdir)
    mkdir(joinpath(vdir, "lib"))
end

LDFLAGS = "-Wl,-rpath,$vdir/lib -Wl,-rpath,\$\$ORIGIN/../share/julia/site/v$(VERSION.major).$(VERSION.minor)/Nemo/local/lib"
DLCFLAGS = "-fPIC -fno-common"

cd(wdir)

function download_dll(url_string, location_string)
   try
      run(`curl -o $(location_string) -L $(url_string)`)
   catch
      download(url_string, location_string)
   end
end

#install libpthreads


if Sys.iswindows()
   println("Downloading libpthread ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libwinpthread-1.dll", joinpath(vdir, "lib", "libwinpthread-1.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libwinpthread-1.dll", joinpath(vdir, "lib", "libwinpthread-1.dll"))
   end
   println("DONE")
end

cd(wdir)

# install M4

if !Sys.iswindows()
   try
      run(`m4 --version`)
   catch
      println("Building m4 ... ")
      M4_FILE = "m4-" * M4_VERSION * ".tar.bz2"
      download("http://ftp.gnu.org/gnu/m4/$M4_FILE", joinpath(wdir, "$M4_FILE"))
      run(`tar -xvf $M4_FILE`)
      run(`rm $M4_FILE`)
      cd(joinpath("$wdir", "m4-$M4_VERSION"))
      run(`./configure --prefix=$vdir`)
      run(`make`)
      run(`make install`)
      println("DONE")
   end
end

cd(wdir)

# install yasm

if !Sys.iswindows()
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
end

cd(wdir)

# install GMP/MPIR

MPIR_FILE = "mpir-" * MPIR_VERSION * ".tar.bz2"

if !ispath(joinpath(wdir, "mpir-$MPIR_VERSION"))
   println("Downloading MPIR sources ... ")
   download("http://nemocas.org/binaries/$MPIR_FILE", joinpath(wdir, MPIR_FILE))
   println("DONE")
end

if Sys.iswindows()
   println("Downloading MPIR ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libgmp-16.dll", joinpath(vdir, "lib", "libgmp-16.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libgmp-16.dll", joinpath(vdir, "lib", "libgmp-16.dll"))
   end
   println("DONE")
else
   println("Building MPIR ... ")
   if isfile(joinpath(wdir, MPIR_FILE))
      run(`tar -xvf $MPIR_FILE`)
      run(`rm $MPIR_FILE`)
   end
   cd("$wdir/mpir-$MPIR_VERSION")
   try
      run(`m4 --version`)
      run(`./configure --with-yasm=$wdir/yasm-$YASM_VERSION/yasm --prefix=$vdir --enable-gmpcompat --disable-static --enable-shared`)
   catch
      run(`./configure --with-yasm=$wdir/yasm-$YASM_VERSION/yasm --prefix=$vdir M4=$vdir/bin/m4 --enable-gmpcompat --disable-static --enable-shared`)
   end
   run(`make -j4`)
   run(`make install`)
   cd(wdir)
   run(`rm -rf bin`)
   println("DONE")
end

cd(wdir)

# install MPFR

MPFR_FILE = "mpfr-" * MPFR_VERSION * ".tar.bz2"

if !ispath(joinpath(wdir, "mpfr-$MPFR_VERSION"))
   println("Downloading MPFR sources ... ")
   download("http://ftp.vim.org/ftp/gnu/mpfr/$MPFR_FILE", joinpath(wdir, MPFR_FILE))

   println("DONE")
end

if Sys.iswindows()
   println("Downloading MPFR ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libmpfr-4.dll", joinpath(vdir, "lib", "libmpfr-4.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libmpfr-4.dll", joinpath(vdir, "lib", "libmpfr-4.dll"))
   end
   println("DONE")
else
   println("Building MPFR ... ")
   if isfile(joinpath(wdir, MPFR_FILE))
      run(`tar -xvf $MPFR_FILE`)
      run(`rm $MPFR_FILE`)
   end
   cd("$wdir/mpfr-$MPFR_VERSION")
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --with-gmp=$vdir --disable-static --enable-shared`) 
      run(`make -j4`)
      run(`make install`)
   end
   cd(wdir)
   println("DONE")
end

cd(wdir)

# install FLINT
if !Sys.iswindows()
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
end

if Sys.iswindows()
   println("Downloading flint ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libflint.dll", joinpath(vdir, "lib", "libflint.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libflint.dll.$FLINT_VERSION", joinpath(vdir, "lib", "libflint.dll"))
   end
   try
      run(`ln -sf $vdir\\lib\\libflint.dll $vdir\\lib\\libflint-13.dll`)
   catch
      cp(joinpath(vdir, "lib", "libflint.dll"), joinpath(vdir, "lib", "libflint-13.dll"), force = true)
   end
   println("DONE")
else
   println("Building flint ... ")
   cd(joinpath("$wdir", "flint2"))
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --disable-static --enable-shared --with-mpir=$vdir --with-mpfr=$vdir`) 
      run(`make -j4`)
      run(`make install`)
   end
   println("DONE")
end

cd(wdir)

# INSTALL ARB 

if !Sys.iswindows()
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
  #open(`patch --forward -d arb -r -`, "r", open("../deps-PIE-ftbfs.patch"))
  println("DONE")
end

 # install ANTIC

if !Sys.iswindows()
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
  #open(`patch --forward -d antic -r -`, "r", open("../deps-PIE-ftbfs.patch"))
  println("DONE")
end

cd(wdir)

if Sys.iswindows()
   println("Downloading arb ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libarb.dll", joinpath(vdir, "lib", "libarb.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libarb.dll.$ARB_VERSION", joinpath(vdir, "lib", "libarb.dll"))
   end
   println("DONE")
else
   println("Building arb ... ")
   cd(joinpath("$wdir", "arb"))
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --disable-static --enable-shared --with-mpir=$vdir --with-mpfr=$vdir --with-flint=$vdir`)
      run(`make -j4`)
      run(`make install`)
   end
   println("DONE")
end

if Sys.iswindows()
   println("Downloading antic ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libantic.dll", joinpath(vdir, "lib", "libantic.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libantic.dll", joinpath(vdir, "lib", "libantic.dll"))
   end
   println("DONE")
else
   println("Building antic ... ")
   cd(joinpath("$wdir", "antic"))
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --disable-static --enable-shared --with-mpir=$vdir --with-mpfr=$vdir --with-flint=$vdir`)
      run(`make -j4`)
      run(`make install`)
   end
   println("DONE")
end

cd(wdir)

push!(Libdl.DL_LOAD_PATH, joinpath(vdir, "lib"))

cd(oldwdir)
