oldwdir = pwd()

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
      download("http://ftp.gnu.org/gnu/m4/m4-1.4.17.tar.bz2", joinpath(wdir, "m4-1.4.17.tar.bz2"))
      run(`tar -xvf m4-1.4.17.tar.bz2`)
      run(`rm m4-1.4.17.tar.bz2`)
      cd(joinpath("$wdir", "m4-1.4.17"))
      run(`./configure --prefix=$vdir`)
      run(`make`)
      run(`make install`)
      println("DONE")
   end
end

cd(wdir)

# install yasm

if !is_windows()
   if !ispath(joinpath(wdir, "yasm-1.3.0"))
      println("Building yasm ... ")
      download("http://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz", "yasm-1.3.0.tar.gz")
      run(`tar -xvf yasm-1.3.0.tar.gz`)
      run(`rm yasm-1.3.0.tar.gz`)
      cd(joinpath("$wdir","yasm-1.3.0"))
      run(`./configure`)
      run(`make`)
      println("DONE")
   end
end

cd(wdir)

# install GMP/MPIR

if !ispath(joinpath(wdir, "mpir-3.0.0"))
   println("Downloading MPIR sources ... ")
   download("http://mpir.org/mpir-3.0.0.tar.bz2", joinpath(wdir, "mpir-3.0.0.tar.bz2"))
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
   if isfile(joinpath(wdir, "mpir-3.0.0.tar.bz2"))
      run(`tar -xvf mpir-3.0.0.tar.bz2`)
      run(`rm mpir-3.0.0.tar.bz2`)
   end
   cd("$wdir/mpir-3.0.0")
   try
      run(`m4 --version`)
      run(`./configure --with-yasm=$wdir/yasm-1.3.0/yasm --prefix=$vdir --enable-gmpcompat --disable-static --enable-shared`)
   catch
      run(`./configure --with-yasm=$wdir/yasm-1.3.0/yasm --prefix=$vdir M4=$vdir/bin/m4 --enable-gmpcompat --disable-static --enable-shared`)
   end
   run(`make -j4`)
   run(`make install`)
   cd(wdir)
   run(`rm -rf bin`)
   println("DONE")
end

cd(wdir)

# install MPFR

if !ispath(joinpath(wdir, "mpfr-3.1.6"))
   println("Downloading MPFR sources ... ")
   download("http://ftp.gnu.org/gnu/mpfr/mpfr-3.1.6.tar.bz2", joinpath(wdir, "mpfr-3.1.6.tar.bz2"))

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
   if isfile(joinpath(wdir, "mpfr-3.1.6.tar.bz2"))
      run(`tar -xvf mpfr-3.1.6.tar.bz2`)
      run(`rm mpfr-3.1.6.tar.bz2`)
   end
   cd("$wdir/mpfr-3.1.6")
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
    run(`git checkout 119d15d686436d94f39fd3badf5eea4acf94ab72`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "antic"))
      cd(joinpath("$wdir", "antic"))
      run(`git fetch origin`)
      run(`git checkout 119d15d686436d94f39fd3badf5eea4acf94ab72`)
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
    run(`git checkout da93dd4ff280697376a65684408c1a245f2155cb`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "flint2"))
       cd(joinpath("$wdir", "flint2"))
       run(`git fetch`)
       run(`git checkout da93dd4ff280697376a65684408c1a245f2155cb`)
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
    run(`git checkout 6035ee2420b7a3fa0259c92dcfa5de4bc76a4b95`)
    cd(wdir)
  catch
    if ispath(joinpath("$wdir", "arb"))
      cd(joinpath("$wdir", "arb"))
      run(`git fetch`)
      run(`git checkout 6035ee2420b7a3fa0259c92dcfa5de4bc76a4b95`)
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
