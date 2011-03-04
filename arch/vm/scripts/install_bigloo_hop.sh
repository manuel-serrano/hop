#! /bin/bash

set -e

if [ $# -lt 3 ]; then
   echo "Usage: $0 img_file bigloo_version hop_version [other_pkg,...]"
   echo
   echo "    Example: $0 /tmp/hop.hdd 3.6b 2.2.1 hoptex,popart-api -- gource,hgview"
   echo
   echo "  hop_version can be in the form 'hg;url;revision'"
   echo "  which clones the repo at 'url' and checkouts revision/tag 'revision'."
   echo "  take in account that the repo should allow anonymous checkouts,"
   echo "  as it won't have your credentials to clone."
   echo "  you can use 'hg serve' in any updated repo and use 'http://localhost:8000/' as url."
   echo
   exit 1
fi

# params
script_dir=`dirname $0`
img_file=$1
img_dir=`dirname $img_file`
mnt_dir=$img_dir/mnt

bigloo_version=$2
hop_version=$3
shift 3

while : ; do
  case $1 in;
    --)
     other_pkgs="$@"
     ;;
    *)
     extra_welbets="$extra_welbets $1";
     ;;
  esac
  shift
done

mkdir -pv $mnt_dir

# failsafe
mount_points=""
loop_devices=""
disengage () {
   echo
   echo -n "Disengage! "
   for mpt in $mount_points; do
      umount $mpt
   done
   # loop: can't delete device /dev/loop0: Device or resource busy
   for dev in $loop_devices; do
      sleep 3
      losetup -d $dev
   done
   echo "done."
}

trap disengage EXIT

echo -n "Mounting..."
losetup /dev/loop0 $img_file
loop_devices="/dev/loop0 $loop_devices"
losetup -o 32256 /dev/loop1 /dev/loop0
loop_devices="/dev/loop1 $loop_devices"

mount /dev/loop1 $mnt_dir/
echo -n "$img_file "
mount_points="$mnt_dir $mount_points"

for mntpt in /dev /dev/pts /proc; do
   echo -n "$mntpt "
   mkdir -p $mnt_dir/$mntpt
   mount -o bind $mntpt $mnt_dir/$mntpt
   mount_points="$mnt_dir/$mntpt $mount_points"
done
echo "done."

echo "Chrootin'..."
cp -v /etc/resolv.conf $mnt_dir/etc/resolv.conf
cp -v $script_dir/compile_bigloo_hop.sh $mnt_dir/root/compile_bigloo_hop.sh
chroot $mnt_dir bash /root/compile_bigloo_hop.sh "$bigloo_version" "$hop_version" $other_pkgs

echo "Extra weblets"
for p in $extra_weblets; do
  cp $extra_weblets $mnt_dir/home/hop/.config/hop/weblets
done
