using BinaryProvider # requires BinaryProvider 0.3.0 or later

# Parse some basic command-line arguments
const verbose = "--verbose" in ARGS
const prefix = Prefix(get([a for a in ARGS if a != "--verbose"], 1, joinpath(@__DIR__, "usr")))
products = [
    LibraryProduct(prefix, ["libarb"], :libarb),
]

# Download binaries from hosted location
bin_prefix = "https://github.com/JuliaBinaryWrappers/Arb_jll.jl/releases/download/Arb-v200.2000.0+0"

# Listing of files generated by BinaryBuilder:
download_info = Dict(
    Linux(:aarch64, libc=:glibc) => ("$bin_prefix/Arb.v200.2000.0.aarch64-linux-gnu.tar.gz", "4d8098a347fe2c1601896566f2dd6daeb63ad6e9caad985ef3a30eab784f9c13"),
    Linux(:aarch64, libc=:musl) => ("$bin_prefix/Arb.v200.2000.0.aarch64-linux-musl.tar.gz", "f0ab9414b3dcad02d5400a510b67448587c502d17e29c71802cb3e559b37d258"),
    Linux(:armv7l, libc=:glibc, call_abi=:eabihf) => ("$bin_prefix/Arb.v200.2000.0.armv7l-linux-gnueabihf.tar.gz", "bdad39914750ceec5a5e524013575946a37694ca394fc6842d6ca1517f29860a"),
    Linux(:armv7l, libc=:musl, call_abi=:eabihf) => ("$bin_prefix/Arb.v200.2000.0.armv7l-linux-musleabihf.tar.gz", "b008347cdd68428b637010e7420dd1f607a19a636c82acd07c60fff91faf88f7"),
    Linux(:i686, libc=:glibc) => ("$bin_prefix/Arb.v200.2000.0.i686-linux-gnu.tar.gz", "d8653f50c31bd633aafe9cb8632660ab8b7aa592582034568759d8c0941e4f40"),
    Linux(:i686, libc=:musl) => ("$bin_prefix/Arb.v200.2000.0.i686-linux-musl.tar.gz", "e6e63b7a0529507b5d4cd9c5a30ea8b41ab143eb5b7988b0197d6fde2b130d4f"),
    Windows(:i686) => ("$bin_prefix/Arb.v200.2000.0.i686-w64-mingw32.tar.gz", "e60de773bf5bbde2f814d9177845babdcd51c0e7b03ca928ed710705a056b376"),
    Linux(:powerpc64le, libc=:glibc) => ("$bin_prefix/Arb.v200.2000.0.powerpc64le-linux-gnu.tar.gz", "6f6adc3365e015ba3943be34d3645978134ffb6dd0b4e0324d2a366510b06fa8"),
    MacOS(:x86_64) => ("$bin_prefix/Arb.v200.2000.0.x86_64-apple-darwin.tar.gz", "9c6b7e3cff5bc7626d086289ac25a3d26697e24c1f7b565341d4d7e1973646ee"),
    Linux(:x86_64, libc=:glibc) => ("$bin_prefix/Arb.v200.2000.0.x86_64-linux-gnu.tar.gz", "b7fea75e4d4edfeebb3de4142211c4ced07af44ca4e98477d72f1e6c631d5b95"),
    Linux(:x86_64, libc=:musl) => ("$bin_prefix/Arb.v200.2000.0.x86_64-linux-musl.tar.gz", "482b23667a18423c1ea9bffc6b52f8738ddb17373c8962c1486ab841ca4bdd24"),
    FreeBSD(:x86_64) => ("$bin_prefix/Arb.v200.2000.0.x86_64-unknown-freebsd.tar.gz", "4e4d6c5d89be751ce2292294637da99ecde9029c399c1a38ca13e65ece2d3361"),
    Windows(:x86_64) => ("$bin_prefix/Arb.v200.2000.0.x86_64-w64-mingw32.tar.gz", "ccc64d76cf65ca310a1508d8523bc799faab047fe0842556749a54ef120f23d4"),
)

# Install unsatisfied or updated dependencies:
unsatisfied = any(!satisfied(p; verbose=verbose) for p in products)
dl_info = choose_download(download_info, platform_key_abi())
if dl_info === nothing && unsatisfied
    # If we don't have a compatible .tar.gz to download, complain.
    # Alternatively, you could attempt to install from a separate provider,
    # build from source or something even more ambitious here.
    error("Your platform (\"$(Sys.MACHINE)\", parsed as \"$(triplet(platform_key_abi()))\") is not supported by this package!")
end

# If we have a download, and we are unsatisfied (or the version we're
# trying to install is not itself installed) then load it up!
if unsatisfied || !isinstalled(dl_info...; prefix=prefix)
    # Download and install binaries
    install(dl_info...; prefix=prefix, force=true, verbose=verbose)
end

# Write out a deps.jl file that will contain mappings for our products
write_deps_file(joinpath(@__DIR__, "deps.jl"), products, verbose=verbose)