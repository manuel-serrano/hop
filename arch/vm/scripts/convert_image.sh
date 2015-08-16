#! /bin/bash
# Depends: qemu-utils

set -e

if [ $# -lt 1 ]; then
   echo "Usage: $0 img_file"
   exit 1
fi

# params
img_filename=$1

raw_img="$img_filename.hdd"
vmdk_img="$img_filename.vmdk"

echo
echo -n "Converting to VMDK... (takes a while, yes..) "
qemu-img convert -O vmdk $raw_img $vmdk_img
echo "done."

