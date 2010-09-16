arch_dir=$(dirname $0)
conf_file=$arch_dir/config-local.sh

if [ -f $conf_file ]; then
   source $conf_file
else
   echo "config file '$conf_file' not found, bailing out."
   exit 1
fi

if ! [ -d "$ANDROIDROOT" -a -d "$BS_BIGLOO" ]; then
   echo "config seems wrong. check config file $conf_file"
   exit 1
fi

# Android NDK
export ANDNDK=$ANDROIDROOT/android-ndk-r4b

# the prefix where the host bigloo is installed
export BGL_PREFIX=$BS_BIGLOO
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$BS_HOPDIR/lib
