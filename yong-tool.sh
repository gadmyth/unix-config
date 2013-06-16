#!/bin/bash
# yong input method
# author dgod

usage()
{
	echo "yong-tool.sh [OPTION]"
	echo "  --install		install yong at system"
	echo "  --uninstall		uninstall yong from system"
	echo "  --select		select yong as default IM"
}

fedora_install()
{
	CFG=/etc/X11/xinit/xinput.d/yong.conf
	rm -rf $CFG
	touch $CFG
	echo 'DISABLE_IMSETTINGS=true' >> $CFG
	echo 'XMODIFIERS="@im=yong"' >> $CFG
	echo 'XIM="yong"' >> $CFG
	echo 'XIM_PROGRAM="/usr/bin/yong"' >> $CFG
	echo 'XIM_ARGS="-d"' >> $CFG
	echo 'GTK_IM_MODULE="xim"' >> $CFG
	echo 'QT_IM_MODULE="xim"' >> $CFG
	echo 'SHORT_DESC="yong"' >> $CFG
	echo 'PREFERENCE_PROGRAM="/usr/bin/yong-setup"' >> $CFG
}

fedora_uninstall()
{
	CFG=/etc/X11/xinit/xinput.d/yong.conf
	rm -rf $CFG
}

fedora_select()
{
	CFG=/etc/X11/xinit/xinput.d/yong.conf
	ln -sf $CFG ~/.xinputrc
}

debian_install()
{
	CFG=/etc/X11/xinit/xinput.d/yong
	rm -rf $CFG
	touch $CFG
	echo 'XMODIFIERS="@im=yong"' >> $CFG
	echo 'XIM="yong"' >> $CFG
	echo 'XIM_PROGRAM="/usr/bin/yong"' >> $CFG
	echo 'XIM_ARGS="-d"' >> $CFG
	echo 'GTK_IM_MODULE="xim"' >> $CFG
	echo 'QT_IM_MODULE="xim"' >> $CFG
	echo 'PREFERENCE_PROGRAM="/usr/bin/yong-setup"' >> $CFG
}

debian_uninstall()
{
	CFG=/etc/X11/xinit/xinput.d/yong
	rm -rf $CFG
}

debian_select()
{
	im-switch -s yong
}

suse_install()
{
	CFG=/etc/X11/xim.d/yong
	rm -rf $CFG
	touch $CFG
	echo 'XMODIFIERS="@im=yong"' >> $CFG
	echo 'XIM="yong"' >> $CFG
	echo 'XIM_PROGRAM="/usr/bin/yong"' >> $CFG
	echo 'XIM_ARGS="-d"' >> $CFG
	echo 'GTK_IM_MODULE="xim"' >> $CFG
	echo 'QT_IM_MODULE="xim"' >> $CFG
	echo 'PREFERENCE_PROGRAM="/usr/bin/yong-setup"' >> $CFG
}

suse_uninstall()
{
	CFG=/etc/X11/xim.d/yong
	rm -rf $CFG
}

suse_select()
{
	CFG=~/.xim
	rm -rf $CFG
	touch $CFG
	echo 'export XMODIFIERS="@im=yong"' >> $CFG
	echo 'export XIM="yong"' >> $CFG
	echo 'export XIM_PROGRAM="/usr/bin/yong"' >> $CFG
	echo 'export XIM_ARGS="-d"' >> $CFG
	echo 'export GTK_IM_MODULE="xim"' >> $CFG
	echo 'export QT_IM_MODULE="xim"' >> $CFG
	echo 'export PREFERENCE_PROGRAM="/usr/bin/yong-setup"' >> $CFG
	echo 'yong -d' >> $CFG
}

arch_install()
{
	CFG=/etc/X11/xinit/xinput.d/yong
	rm -rf $CFG
	touch $CFG
	echo 'export XMODIFIERS="@im=yong"' >> $CFG
	echo 'export XIM="yong"' >> $CFG
	echo 'export XIM_PROGRAM="/usr/bin/yong"' >> $CFG
	echo 'export XIM_ARGS="-d"' >> $CFG
	echo 'export GTK_IM_MODULE="xim"' >> $CFG
	echo 'export QT_IM_MODULE="xim"' >> $CFG
	echo 'export PREFERENCE_PROGRAM="/usr/bin/yong-setup"' >> $CFG
}

arch_uninstall()
{
	CFG=/etc/X11/xinit/xinput.d/yong
	rm -rf $CFG
}

arch_select()
{
	CFG=/etc/X11/xinit/xinput.d/yong
	echo $CFG
	sudo ln -sf $CFG ~/.xinputrc
}

legacy_install()
{
	CFG=~/.xinitrc
	rm -rf $CFG
	touch $CFG
	echo 'export XMODIFIERS="@im=yong"' >> $CFG
	echo 'export GTK_IM_MODULE="xim"' >> $CFG
	echo 'export QT_IM_MODULE="xim"' >> $CFG
	echo 'yong -d' >> $CFG
}

legacy_uninstall()
{
	CFG=~/.xinitrc
	echo 'please clean yong at ~/.xinitrc yourself'
}

legacy_select()
{
	CFG=~/.xinitrc
}

mandriva_install()
{
	CFG=~/.i18n
}

mandriva_uninstall()
{
	CFG=~/.i18n
	rm -rf $CFG
}

mandriva_select()
{
	CFG=~/.i18n
	rm -rf $CFG
	touch $CFG
	echo 'export XIM=yong' >> $CFG
	echo 'export XIM_PROGRAM=yong' >> $CFG
	echo 'export XMODIFIERS="@im=yong"' >> $CFG
	echo 'export GTK_IM_MODULE="xim"' >> $CFG
	echo 'export QT_IM_MODULE="xim"' >> $CFG
}

ibus_install()
{
	IBUS_D=/usr/share/ibus/component
	if [ -d $IBUS_D ] ; then
		sed "s%\/usr\/share\/yong%`pwd`%" yong.xml >$IBUS_D/yong.xml
	fi
}

ibus_uninstall()
{
	if [ -f /usr/share/ibus/component/yong.xml ] ; then
		rm -f /usr/share/ibus/component/yong.xml
	fi
}

if [ $# != 1 ] ; then
	usage
	exit 0
fi

DIST=none

# detect mandriva first, fo mandriva provides redhat-release
if [ -f /etc/mandriva-release ] ; then
	DIST=mandriva
elif [ -f /etc/fedora-release ] ; then
	DIST=fedora
elif [ -f /etc/redhat-release ] ; then
	DIST=fedora
elif [ -f /etc/centos-release ] ; then
	DIST=fedora
elif [ -f /etc/debian-release ] ; then
	DIST=debian
elif [ -f /etc/SuSE-release ] ; then
	DIST=suse
elif [ -f /etc/arch-release ] ; then
	DIST=arch
elif [ `cat /etc/issue | grep Ubuntu | wc -l` != 0 ] ; then
	DIST=debian
elif [ `cat /etc/issue | grep Puppy | wc -l` != 0 ] ; then
	DIST=legacy
fi

echo DIST $DIST found

if [ $DIST = "none" ] ; then
	exit 1
fi

if [ $1 = "--install" ] ; then
	ln -sf `pwd`/yong /usr/bin/yong
	ln -sf `pwd`/yong-setup /usr/bin/yong-setup
	ibus_install
	if [ $DIST = "fedora" ] ; then
		fedora_install
	elif [ $DIST = "debian" ] ; then
		debian_install
	elif [ $DIST = "suse" ] ; then
		suse_install
	elif [ $DIST = "arch" ] ; then
		arch_install
	elif [ $DIST = "mandriva" ] ; then
		mandriva_install
	elif [ $DIST = "legacy" ] ; then
		legacy_install
	fi
elif [ $1 = "--uninstall" ] ; then
	rm -rf /usr/bin/yong
	rm -rf /usr/bin/yong-setup
	ibus_uninstall
	if [ $DIST = "fedora" ] ; then
		fedora_uninstall
	elif [ $DIST = "debian" ] ; then
		debian_uninstall
	elif [ $DIST = "suse" ] ; then
		suse_uninstall
	elif [ $DIST = "arch" ] ; then
		arch_uninstall
	elif [ $DIST = "mandriva" ] ; then
		mandriva_uninstall
	elif [ $DIST = "legacy" ] ; then
		legacy_uninstall
	fi
elif [ $1 = "--select" ] ; then
	if [ $DIST = "fedora" ] ; then
		fedora_select
	elif [ $DIST = "debian" ] ; then
		debian_select
	elif [ $DIST = "suse" ] ; then
		suse_select
	elif [ $DIST = "arch" ] ; then
		arch_select
	elif [ $DIST = "mandriva" ] ; then
		mandriva_select
	elif [ $DIST = "legacy" ] ; then
		legacy_select
	fi
fi

echo "$1 Done"

