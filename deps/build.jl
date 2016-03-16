on_windows = @windows ? true : false
on_osx = @osx ? true : false

oldwdir = pwd()

pkgdir = Pkg.dir("Nemo") 
wdir = Pkg.dir("Nemo", "deps")
vdir = Pkg.dir("Nemo", "local")

if !ispath(Pkg.dir("Nemo", "local"))
    mkdir(Pkg.dir("Nemo", "local"))
end
if !ispath(Pkg.dir("Nemo", "local", "lib"))
    mkdir(Pkg.dir("Nemo", "local", "lib"))
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

if on_windows
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libwinpthread-1.dll", joinpath(vdir, "lib", "libwinpthread-1.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libwinpthread-1.dll", joinpath(vdir, "lib", "libwinpthread-1.dll"))
   end
end

cd(wdir)

# install M4

if !on_windows
   try
      run(`m4 --version`)
   catch
      download("http://ftp.gnu.org/gnu/m4/m4-1.4.17.tar.bz2", joinpath(wdir, "m4-1.4.17.tar.bz2"))
      run(`tar -xvf m4-1.4.17.tar.bz2`)
      run(`rm m4-1.4.17.tar.bz2`)
      cd("$wdir/m4-1.4.17")
      run(`./configure --prefix=$vdir`)
      run(`make`)
      run(`make install`)
   end
end

cd(wdir)

# install GMP/MPIR

if !ispath(Pkg.dir("Nemo", "local", "mpir-2.7.2"))
   download("http://mpir.org/mpir-2.7.2.tar.bz2", joinpath(wdir, "mpir-2.7.2.tar.bz2"))
end

if on_windows
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libgmp-16.dll", joinpath(vdir, "lib", "libgmp-16.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libgmp-16.dll", joinpath(vdir, "lib", "libgmp-16.dll"))
   end
else
   run(`tar -xvf mpir-2.7.2.tar.bz2`)
   run(`rm mpir-2.7.2.tar.bz2`)
   cd("$wdir/mpir-2.7.2")
   try
      run(`m4 --version`)
      run(`./configure --prefix=$vdir --enable-gmpcompat --disable-static --enable-shared`)
   catch
      run(`./configure --prefix=$vdir M4=$vdir/bin/m4 --enable-gmpcompat --disable-static --enable-shared`)
   end
   run(`make -j4`)
   run(`make install`)
   cd(wdir)
   run(`rm -rf bin`)
end

cd(wdir)

# install MPFR

if !ispath(Pkg.dir("Nemo", "local", "mpfr-3.1.4"))
   download("http://ftp.gnu.org/gnu/mpfr/mpfr-3.1.4.tar.bz2", joinpath(wdir, "mpfr-3.1.4.tar.bz2"))
end

if on_windows
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libmpfr-4.dll", joinpath(vdir, "lib", "libmpfr-4.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libmpfr-4.dll", joinpath(vdir, "lib", "libmpfr-4.dll"))
   end
else
   run(`tar -xvf mpfr-3.1.4.tar.bz2`)
   run(`rm mpfr-3.1.4.tar.bz2`)
   cd("$wdir/mpfr-3.1.4")
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --with-gmp=$vdir --disable-static --enable-shared`) 
      run(`make -j4`)
      run(`make install`)
   end
   cd(wdir)
end

cd(wdir)

# install ANTIC

try
  run(`git clone https://github.com/wbhart/antic.git`)
catch
  cd("$wdir/antic")
  run(`git pull`)
end          

cd(wdir)

# install FLINT
try
  run(`git clone https://github.com/wbhart/flint2.git`)
catch
  cd("$wdir/flint2")
  run(`git pull`)
end          

if on_windows
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libflint.dll", joinpath(vdir, "lib", "libflint.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libflint.dll", joinpath(vdir, "lib", "libflint.dll"))
   end
   try
      symlink(joinpath(vdir, "lib", "libflint.dll"), joinpath(vdir, "lib", "libflint-13.dll"))
   catch
      cp(joinpath(vdir, "lib", "libflint.dll"), joinpath(vdir, "lib", "libflint-13.dll"))
   end
else
   cd("$wdir/flint2")
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --extensions="$wdir/antic" --disable-static --enable-shared --with-mpir=$vdir --with-mpfr=$vdir`) 
      run(`make -j4`)
      run(`make install`)
   end
end

cd(wdir)

# INSTALL ARB 

try
  run(`git clone https://github.com/fredrik-johansson/arb.git`)
catch
  cd("$wdir/arb")
  run(`git pull`)
  cd(wdir)
end          
 
if on_windows
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libarb.dll", joinpath(vdir, "lib", "libarb.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libarb.dll", joinpath(vdir, "lib", "libarb.dll"))
   end
else
   cd("$wdir/arb")
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "LDFLAGS"=>LDFLAGS) do
      run(`./configure --prefix=$vdir --disable-static --enable-shared --with-mpir=$vdir --with-mpfr=$vdir --with-flint=$vdir`)
      run(`make -j4`)
      run(`make install`)
   end
end

cd(wdir)

# install PARI

if !ispath(Pkg.dir("Nemo", "local", "pari-2.7.4"))
   # git clone pari doesn't seem to work on Windows
   # bison is too old on OSX for pari git
   # so we use the 2.7.4 tarball

   download("http://pari.math.u-bordeaux.fr/pub/pari/unix/pari-2.7.4.tar.gz", joinpath(wdir, "pari-2.7.4.tar.gz"))
end

if on_windows
   if Int == Int32
      download_dll("http://nemocas.org/binaries/w32-libpari.dll", joinpath(vdir, "lib", "libpari.dll"))
   else
      download_dll("http://nemocas.org/binaries/w64-libpari.dll", joinpath(vdir, "lib", "libpari.dll"))
   end
elseif on_osx
   run(`tar -xvf pari-2.7.4.tar.gz`)
   run(`rm pari-2.7.4.tar.gz`)
   cd("$wdir/pari-2.7.4")
   run(`patch -p1 -i ../patch-alloc-2.7.4`)
   withenv("DYLD_LIBRARY_PATH"=>"$vdir/lib", "DLCFLAGS"=>DLCFLAGS) do
      run(`./Configure --prefix=$vdir --with-gmp=$vdir`)
      run(`make -j4 gp`)
      run(`make install`)
   end
else
   run(`tar -xvf pari-2.7.4.tar.gz`)
   run(`rm pari-2.7.4.tar.gz`)
   cd("$wdir/pari-2.7.4")
   run(`patch -p1 -i ../patch-alloc-2.7.4`)
   withenv("LD_LIBRARY_PATH"=>"$vdir/lib", "DLLDFLAGS"=>LDFLAGS) do
      run(`./Configure --prefix=$vdir --with-gmp=$vdir`)
      run(`make -j4 gp`)
      run(`make install`)
   end
end

cd(wdir)

push!(Libdl.DL_LOAD_PATH, Pkg.dir("Nemo", "local", "lib"))

cd(oldwdir)

