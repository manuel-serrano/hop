#include <CoreFoundation/CoreFoundation.h>
#include <string.h>

/* many thanks go to Benjamin Prucha for providing this code. */

static
int resourceDirFill(char *buffer, int size) {
    CFBundleRef br = CFBundleGetMainBundle();
    CFURLRef ur = CFBundleCopyResourcesDirectoryURL(br);
    CFStringRef sr = CFURLCopyFileSystemPath(ur, kCFURLPOSIXPathStyle);
    Boolean succeeded = CFURLGetFileSystemRepresentation(ur, true, (UInt8*) buffer, (CFIndex) size);
    CFRelease(ur); CFRelease(sr);
    if (succeeded) return strlen(buffer);
    return -1;
}
