#! /bin/bash
# Depends: qemu-utils parted e2fsprogs syslinux/extlinux

set -e

export LC_ALL=en_US.UTF-8
for i in LC_PAPER LC_MONETARY LC_NUMERIC LC_MESSAGES LC_COLLATE LC_CTYPE LC_TIME; do
    unset $i
done

create_install=1

if [ "$1" == "--config-only" ]; then
   shift
   create_install=0
fi

if [ $# -lt 2 ]; then
   echo "Usage: $0 [--config-only] img_file img_size [pkg,...]"
   echo
   echo "  --config-only: Only configure the image."
   echo "                 Usefull for reconfiguring images already created."
   echo "                 In this case img_size is ignored."
   echo
   echo "  Note: A basic Debian image is about 700MiB."
   echo "        You need 200MiB for compiling Bigloo and another 30 for Hop."
   echo
   exit 1
fi

# check /dev/loop first
if [ ! -b /dev/loop0 ]; then
  echo "*** ERROR: /dev/loop0 does not exist"
  echo "check if the kernel \"loop\" module is correctly loaded."
  echo "Try \"sudo modprobe loop\" to load it."
  exit 1
fi

# params
raw_img=$1
img_size=$2
img_dir=`dirname $raw_img`
mnt_dir=$img_dir/mnt

# as a comma separated list
other_pkgs=$3

# config
mirror="http://ftp.fr.debian.org/debian"

mkdir -pv $mnt_dir

# failsafe
mount_points=""
loop_devices=""
disengage () {
   echo
   echo "Disengage!"
   for mpt in $mount_points; do
      umount -v $mpt
   done
   # exit 0
   for dev in $loop_devices; do
      losetup -vd $dev
      # loop: can't delete device /dev/loop0: Device or resource busy
      sleep 5
   done
   echo "done."
}

trap disengage EXIT

# calculate real size
# bytes per sector
bytes=512
# sectors per track
sectors=63
# heads per track
heads=255
# bytes per cylinder is bytes*sectors*head
bpc=$(( bytes*sectors*heads ))
# number of cylinders

case "$img_size" in
   *M|*m)
      # take out the trailing M or m and multiply by a MiB
      img_size=$((${img_size%[Mm]}*1024*1024))
      ;;
   *G|*g)
      # take out the trailing G or g and multiply by a GiB
      img_size=$((${img_size%[Gg]}*1024*1024*1024))
      ;;
esac

cylinders=$(($img_size/$bpc))
# rebound the size
img_size=$(( ($cylinders+1)*$bpc ))

# TODO: test everything is in place
# TODO: if debootstrap is not there, download it?

if [ $create_install -eq 1 ]; then
   echo "Creating disk image $(($img_size/1024/1024)) MiB..."
   # create sparse file
   qemu-img create -f raw $raw_img $img_size

   # 'create' a msdos part table
   # TODO: use mbr-install
   echo -e "\x55\xaa" | dd bs=1 count=2 seek=510 of=$raw_img conv=notrunc

   # only one partition
   # /usr/sbin/grub-setup: warn: This msdos-style partition label has no post-MBR gap; embedding won't be possible!
   #parted -s $raw_img mkpart primary ext2 0 $img_size
   sfdisk -D $raw_img <<EOF
,,L,*
;
;
;
EOF
fi

losetup /dev/loop0 $raw_img
loop_devices="/dev/loop0 $loop_devices"
losetup -o 32256 /dev/loop1 /dev/loop0
loop_devices="/dev/loop1 $loop_devices"

if [ $create_install -eq 1 ]; then
   # format
   mkfs.ext2 /dev/loop1
fi

echo "Mounting $mnt_dir/"
mount /dev/loop1 $mnt_dir/
mount_points="$mnt_dir $mount_points"

if [ $create_install -eq 1 ]; then
   echo
   echo "Debootstraping..."
   # install base and things needed by bigloo
   # TODO: || true?!?
   debootstrap --arch i386 --include=locales,cdbs,debhelper,libsqlite3-dev,\
libssl-dev,libgstreamer-plugins-base0.10-dev,libgmp3-dev,build-essential,\
linux-image-2.6-686,linux-headers-2.6-686,mercurial,samba,psmisc,$other_pkgs stable $mnt_dir $mirror || true
fi

echo
echo -n "Mounting... "
for mntpt in /dev /dev/pts /proc; do
   echo -n "$mntpt "
   mkdir -p $mnt_dir/$mntpt
   mount -o bind $mntpt $mnt_dir/$mntpt
   mount_points="$mnt_dir/$mntpt $mount_points"
done
echo "done."

echo
echo "Basic config..."
# basic config
echo "deb $mirror stable main" > $mnt_dir/etc/apt/sources.list
echo "deb http://security.debian.org stable/updates main" >> $mnt_dir/etc/apt/sources.list

# ---| Configuring linux-image-2.6.26-2-686 |---
# You are attempting to install an initrd kernel image (version 2.6.26-2-686).
# This will not work unless the boot loader is configured to use an initrd.
# This message will appear for any new kernel installation
# unless the following is added to /etc/kernel-img.conf
# do_initrd = Yes
echo "do_initrd = Yes" > $mnt_dir/etc/kernel-img.conf

# perl: warning: Falling back to the standard locale ("C").
echo "en_US.UTF-8 UTF-8" > $mnt_dir/etc/locale.gen

echo
echo "Chrootin'..."
chroot="chroot $mnt_dir"
# {
   $chroot locale-gen
   # fix incomplete installation
   $chroot apt-get update
   $chroot apt-get -f -y --force-yes install
   $chroot apt-get update
   $chroot apt-get -y upgrade
   # Warning: Fake start-stop-daemon called. Doing nothing
   $chroot apt-get -y install --reinstall dpkg
   # MS, 1 march 2010: don't know why it is needed to kill samba!?
   $chroot /etc/init.d/samba stop
   # MS, 4 march 2010: don't know the default root passwd!!!
   $chroot passwd <<EOF
hop
hop
EOF
# }

echo
echo "More config..."
# copy more config
(cd config && tar cf - .) | (cd $mnt_dir && tar xvf -)

# {
   $chroot update-rc.d hop defaults 30
# }

# overwrite the placeholder with root partition's UUID
uuid=$(blkid /dev/loop1 | cut -d '"' -f 2)
kvers=$($chroot ls -1 '/boot/' | grep 'vmlinuz-' | tail -n 1 | sed 's/vmlinuz-//')
conffile=$mnt_dir/boot/syslinux/extlinux.conf
echo
echo -n "Found root partition at UUID=$uuid; configuring extlinux... "
sed -i -e "s/%ROOT%/UUID=$uuid/g; s/%KVERS%/$kvers/g" $conffile
echo "done."

echo
echo -n "Installing mbr... "
for mbr in /usr/lib/extlinux/mbr.bin /usr/lib/syslinux/mbr.bin; do
   if [ -f $mbr ]; then
      echo "found mbr in $mbr... "
      break
   fi
done

dd if=$mbr of=$raw_img conv=notrunc
echo "Installing extlinux... "
extlinux --heads $heads --sectors $sectors --install $mnt_dir/boot/syslinux
echo "done."

# converting to VMDK is done in a separate script

# disengage() is executed at exit anyways
