#!/bin/sh
#*=====================================================================*/
#*    serrano/prgm/project/hop/2.4.x/arch/debian/makedeb.sh            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sat Dec 22 05:37:50 2007                          */
#*    Last change :  Thu Sep 20 08:17:30 2012 (serrano)                */
#*    Copyright   :  2007-12 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Shell script to build the .deb for Hop on Maemo              */
#*    -------------------------------------------------------------    */
#*    Debug with "sh -x makedeb.sh"                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Global configuration                                             */
#*---------------------------------------------------------------------*/
VERSION=2.4.0       # Hop major version e.g. VERSION=2.4.0
MINOR=-pre1         # Hop minor release e.g. MINOR=-pre1
BIGLOOVERSION=3.8d

REPOSITORY=/users/serrano/prgm/distrib
ICONS="hop-16x16.png hop-26x26.png hop-40x40.png hop-64x64.png"
AUTHOR=Manuel.Serrano@inria.fr
LICENSE=gpl
TMP=`pwd`/build.hop
BASEDIR=`dirname $0`
MAEMODIR=$BASEDIR/maemo
HOPPREFIX=/opt/bigloo
PREFIX=/usr
HOPUSER=hop
HOPCONFIGUREOPT=

if [ "$REPODIR " != " " ]; then
  REPOSITORY=$REPODIR;
fi

#*---------------------------------------------------------------------*/
#*    Create the TMP directories                                       */
#*---------------------------------------------------------------------*/
/bin/rm -rf $TMP
mkdir -p $TMP

#*---------------------------------------------------------------------*/
#*    Untar the standard Hop version                                   */
#*---------------------------------------------------------------------*/
tar xfz $REPOSITORY/hop-$VERSION$MINOR.tar.gz -C $TMP

if [ "$MINOR " != " " ]; then
  mv $TMP/hop-$VERSION$MINOR $TMP/hop-$VERSION
fi

#*---------------------------------------------------------------------*/
#*    The maemo configuration                                          */
#*---------------------------------------------------------------------*/
maemo=`pkg-config maemo-version --modversion 2> /dev/null`

if [ $? = 0 ]; then
  debian=maemo
  debianversion=maemo`echo $maemo | sed -e "s/[.].*$//"`
  extradepend="hildon-desktop, "
else
  case `cat /etc/issue | awk '{ print $1 }'` in
    Debian)
      debian=debian;;
    Ubuntu)
      debian=ubuntu;;
    *)
      debian=debian;;
  esac

  debianversion=$debian
  extradepend=
fi

#*---------------------------------------------------------------------*/
#*    Install the maemo specific files (including hop-launcher)        */
#*---------------------------------------------------------------------*/
if [ "$debian " = "maemo " ]; then
  mkdir -p $TMP/hop-$VERSION/maemo
  (cp -r $MAEMODIR/hop-launcher $TMP/hop-$VERSION/maemo)
  (cd $TMP/hop-$VERSION/maemo/hop-launcher && \
   branch=`echo $VERSION | sed -e "s/[0-9]*$/x/g"` && \
   cat configure.in | sed -e "s/@HOPBRANCH@/$branch/g" > configure && \
   cat hop-launcher.rc.in | \
      sed -e "s|@HOPPREFIX@|$HOPPREFIX|g" | \
      sed -e "s|@PREFIX@|$PREFIX|g" > hop-launcher.rc && \
   ./configure)

  mkdir -p $TMP/hop-$VERSION/icons
  for p in $ICONS; do 
    cp $MAEMODIR/$p $TMP/hop-$VERSION/icons;
  done
fi

#*---------------------------------------------------------------------*/
#*    Configure for small devices                                      */
#*---------------------------------------------------------------------*/
cat >> $TMP/hop-$VERSION/etc/hoprc.hop.in <<EOF
;; small device configuration
(hop-max-threads-set! 8)
EOF

# etc/init.d
for p in hop; do
  if [ -f $BASEDIR/init.d/$p.in ]; then
    cat $BASEDIR/init.d/$p.in \
      | sed -e "s/@HOPVERSION@/$VERSION/g" \
            -e "s/@HOPUSER@/$HOPUSER/g" \
            -e "s/@MAEMO@/$maemo/g" \
            -e "s/@DEBIAN@/$debian/g" \
            -e "s/@EXTRADEPEND@/$extradepend/g" \
            -e "s|@HOPPREFIX@|$HOPPREFIX|g" \
            -e "s|@PREFIX@|$PREFIX|g" \
            -e "s/@MAEMOHASLOCATION@/$maemohaslocation/g" \
            -e "s/@BIGLOOVERSION@/$BIGLOOVERSION/g" > \
      $TMP/hop-$VERSION/arch/debian/init.d/$p;
  else
    cp $BASEDIR/init.d/$p $TMP/hop-$VERSION/arch/debian/init.d/$p;
  fi
done

# copyright
cp $TMP/hop-$VERSION/LICENSE $TMP/hop-$VERSION/copyright

#*---------------------------------------------------------------------*/
#*    Create the .tar.gz file used for building the package            */
#*---------------------------------------------------------------------*/
tar cfz $TMP/hop-$VERSION.tar.gz -C $TMP hop-$VERSION

if [ "$debian " = "maemo " ]; then
  cat $MAEMODIR/Makefile.maemo | \
    sed -e "s/@HOPBRANCH@/$branch/g" | \
    sed -e "s|@HOPPREXI@|$HOPREFIX|g" | \
    sed -e "s|@PREFIX@|$PREFIX|g" >> \
    $TMP/hop-$VERSION/Makefile

  echo 'BUILDSPECIFIC=build-maemo' >> \
    $TMP/hop-$VERSION/etc/Makefile.hopconfig.in
    echo 'INSTALLSPECIFIC=install-maemo' >> \
    $TMP/hop-$VERSION/etc/Makefile.hopconfig.in
fi

#*---------------------------------------------------------------------*/
#*    The maemo version                                                */
#*---------------------------------------------------------------------*/
if [ "$debian " = "maemo " ]; then
  maemoversion=`echo $maemo | sed -e "s/[^0-9.].*$//"`

  if [ "$maemoversion " = "5.0 " ]; then
    echo "Configuring for Maemo 5."
    childonflags="pkg-config gtk+-2.0 hildon-1 --cflags"
    ldhildonflags="pkg-config gtk+-2.0 hildon-1 libosso --libs"
    maemo=MAEMO5
    maemohaslocation=no
  else
    pkg-config gtk+-2.0 hildon-1 --cflags > /dev/null 2> /dev/null
    if [ $? = 0 ]; then
      echo "Configuring for Maemo 4."
      childonflags="pkg-config gtk+-2.0 hildon-1 --cflags"
      ldhildonflags="pkg-config gtk+-2.0 hildon-1 --libs"
      maemo=MAEMO4
      maemohaslocation=yes
    else
      echo "Configuring for Maemo 3."
      childonflags="pkg-config gtk+-2.0 hildon-libs --cflags"
      ldhildonflags="pkg-config gtk+-2.0 hildon-libs --libs"
      maemo=MAEMO3
      maemohaslocation=yes
    fi
  fi
fi


#*---------------------------------------------------------------------*/
#*    Start creating the .deb                                          */
#*---------------------------------------------------------------------*/
(cd $TMP/hop-$VERSION &&
 dh_make -c $LICENSE -s -e $AUTHOR -f ../hop-$VERSION.tar.gz <<EOF

EOF
)

# debian specific
for p in control rules postinst postrm; do
  if [ -f $BASEDIR/debian/$p.in ]; then
    cat $BASEDIR/debian/$p.in \
      | sed -e "s/@HOPVERSION@/$VERSION/g" \
            -e "s/@HOPUSER@/$HOPUSER/g" \
            -e "s/@HOPCONFIGUREOPT@/$HOPCONFIGUREOPT/g" \
            -e "s/@MAEMO@/$maemo/g" \
            -e "s/@DEBIAN@/$debian/g" \
            -e "s/@EXTRADEPEND@/$extradepend/g" \
            -e "s|@HOPPREFIX@|$HOPPREFIX|g" \
            -e "s|@PREFIX@|$PREFIX|g" \
            -e "s/@MAEMOHASLOCATION@/$maemohaslocation/g" \
            -e "s/@BIGLOOVERSION@/$BIGLOOVERSION/g" > \
      $TMP/hop-$VERSION/debian/$p;
  else
    cp $BASEDIR/debian/$p $TMP/hop-$VERSION/debian;
  fi
done

# Maemo specific
if [ "$debian " = "maemo " ]; then
  # The desktop file
  cat $MAEMODIR/hop.desktop.in \
    | sed -e "s/@HOPVERSION@/$VERSION/g" \
          -e "s|@HOPPREFIX@|$HOPPREFIX|g" \
          -e "s|@PREFIX@|$PREFIX|g" > \
    $TMP/hop-$VERSION/maemo/hop.desktop && \
    chmod a-w $TMP/hop-$VERSION/maemo/hop.desktop
  cat > $TMP/hop-$VERSION/debian/hop.links <<EOF
usr/share/applications/hildon/hop.desktop etc/others-menu/extra_applications/hop.desktop
EOF

  # The service
  cat $MAEMODIR/hop.service.in  \
    | sed -e "s/@HOPVERSION@/$VERSION/g" \
          -e "s|@HOPPREFIX@|$HOPPREFIX|g" \
          -e "s|@PREFIX@|$PREFIX|g" > \
    $TMP/hop-$VERSION/maemo/hop.service
fi

# The changelog file
/bin/rm -f $TMP/hop-$VERSION/debian/changelog
cat $TMP/hop-$VERSION/ChangeLog | grep -v "^[ \\t]*[.]$$" > \
   $TMP/hop-$VERSION/debian/changelog
cat $BASEDIR/debian/changelog.in | sed "s/@HOPVERSION@/$VERSION/g" > \
   $TMP/hop-$VERSION/debian/changelog

(cd $TMP/hop-$VERSION && dpkg-buildpackage -rfakeroot)

# adjust the package file name
if [ "$MINOR " != " " ]; then
  minor=`echo $MINOR | sed -e "s/-//"`
  for p in $TMP/hop_"$VERSION"_*.deb; do
    np=`echo $p | sed -e "s/${VERSION}/${VERSION}${minor}/"`
    mv $p $np
  done
fi

#*---------------------------------------------------------------------*/
#*    Copy the deb file                                                */
#*---------------------------------------------------------------------*/
if [ -d $REPOSITORY/$debianversion ]; then
  cp $TMP/hop_"$VERSION"*.deb $REPOSITORY/$debianversion
fi
