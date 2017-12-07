oldwdir = pwd()

@show M4_VERSION = "1.4.17"
@show YASM_VERSION = "1.3.0"
@show MPIR_VERSION = "3.0.0"
@show MPFR_VERSION = "3.1.6"
@show ANTIC_VERSION = "fb237532f6772fc04d6d57cc7cf015e444eb2cf4"
@show FLINT_VERSION = "e22c3fc1f040874dbda43baf802c77e59ed9c1a0"
@show ARB_VERSION = "6035ee2420b7a3fa0259c92dcfa5de4bc76a4b95"

pkgdir = dirname(dirname(@__FILE__))
wdir = joinpath(pkgdir, "deps")
vdir = joinpath(pkgdir, "local")

if is_apple() && !("CC" in keys(ENV))
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


if is_windows()
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

if !is_windows()
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

if !is_windows()
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
   download("http://mpir.org/$MPIR_FILE", joinpath(wdir, MPIR_FILE))
   println("DONE")
end

if is_windows()
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
   download("http://ftp.gnu.org/gnu/mpfr/$MPFR_FILE", joinpath(wdir, MPFR_FILE))

   println("DONE")
end

if is_windows()
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

# install ANTIC

if !is_windows()
  println("Cloning antic ... ")
  try
    run(`git clone https://github.com/wbhart/antic.git`)
    cd(joinpath("$wdir", "antic"))
    run(`git checkout $ANTIC_VERSION`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "antic"))
      cd(joinpath("$wdir", "antic"))
      run(`git fetch origin`)
      run(`git checkout $ANTIC_VERSION`)
      cd(wdir)
    end
  end          
  println("DONE")
end

cd(wdir)

# install FLINT
if !is_windows()
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

if is_windows()
   println("Downloading flint ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libflint.dll", joinpath(vdir, "lib", "libflint.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libflint.dll", joinpath(vdir, "lib", "libflint.dll"))
   end
   try
      run(`ln -sf $vdir\\lib\\libflint.dll $vdir\\lib\\libflint-13.dll`)
   catch
      cp(joinpath(vdir, "lib", "libflint.dll"), joinpath(vdir, "lib", "libflint-13.dll"), remove_destination=true)
   end
   println("DONE")
else
   println("Building flint ... ")
   cd(joinpath("$wdir", "flint2"))
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --extensions="$wdir/antic" --disable-static --enable-shared --with-mpir=$vdir --with-mpfr=$vdir`) 
      run(`make -j4`)
      run(`make install`)
   end
   println("DONE")
end

cd(wdir)

# INSTALL ARB 

if !is_windows()
  println("Cloning arb ... ")
  try
    run(`git clone https://github.com/fredrik-johansson/arb.git`)
    cd(joinpath("$wdir", "arb"))
    run(`git checkout $ARB_VERSION`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "arb"))
      open(`patch -R --forward -d arb -r -`, "r", open("../deps-PIE-ftbfs.patch"))
      cd(joinpath("$wdir", "arb"))
      run(`git fetch`)
      run(`git checkout $ARB_VERSION`)
      cd(wdir)
    end
  end
  open(`patch --forward -d arb -r -`, "r", open("../deps-PIE-ftbfs.patch"))
  println("DONE")
end
 
if is_windows()
   println("Downloading arb ... ")
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libarb.dll", joinpath(vdir, "lib", "libarb.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libarb.dll", joinpath(vdir, "lib", "libarb.dll"))
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

cd(wdir)

push!(Libdl.DL_LOAD_PATH, joinpath(vdir, "lib"))

cd(oldwdir)
