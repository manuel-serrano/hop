#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/hop/2.2.x/arch/vm/scripts/mount_hdd.sh      */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Mar  7 07:41:25 2011                          */
#*    Last change :  Mon Mar  7 11:02:58 2011 (serrano)                */
#*    Copyright   :  2011 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    This script is only used for debugging it merely (un)mount       */
#*    the .hdd file.                                                   */
#*=====================================================================*/

action=mount

while : ; do
  case $1 in
    "")
     break;;

    -h|--help)
     echo "usage: $0 [-u|--umount] img [cmd]" >&2;
     exit 1;
     ;;
   
    -u|--umount)
     action="umount";
     ;;

    *)
     if [ "$img_file " = " " ]; then
       img_file=$1;
     else
       cmd=$1;
       action="chroot";
     fi
     ;;
  esac
  shift
done

if [ "$img_file " = " " ]; then
  img_file=/tmp/hop.hdd
fi

if [ "$img_file " = " " ]; then
  echo "usage: $0 [-u|--umount] [-c|--chroot cmd] img" >&2;
  exit 255;
else
  if [ ! -f $img_file ]; then
    echo "*** ERROR: image file does not exists -- $img_file" >&2;
    exit 254;
  fi
  img_dir=`dirname $img_file`
  mnt_dir=$img_dir/mnt  
fi

if [ $action = "mount" -o $action = "chroot" ]; then
  echo "mounting $img_file..."
  losetup /dev/loop0 $img_file
  losetup -o 32256 /dev/loop1 /dev/loop0
  mount /dev/loop1 $mnt_dir/

  for mntpt in /dev /dev/pts /proc; do
    mkdir -p $mnt_dir/$mntpt
    mount -o bind $mntpt $mnt_dir/$mntpt
  done
fi

if [ $action = "chroot" ]; then
  echo "chrooting $mnt_dir $cmd..."
  mv $mnt_dir/etc/resolv.conf $mnt_dir/etc/resolv.conf.orig
  cp -v /etc/resolv.conf $mnt_dir/etc/resolv.conf
  chroot $mnt_dir $cmd
  mv $mnt_dir/etc/resolv.conf.orig $mnt_dir/etc/resolv.conf
fi

if [ $action = "umount" -o $action = "chroot" ]; then
  echo "umounting files..."
  for mntpt in proc dev/pts dev; do
    umount $mnt_dir/$mntpt;
  done

  sleep 1
  umount $mnt_dir
  sleep 3
  echo "deleting loop devices"
  losetup -d /dev/loop1
  sleep 1
  losetup -d /dev/loop0
fi

  
