#!/bin/sh

mkdir tarball && cd tarball

wget "http://l10n.gnome.org/languages/zh_CN/gnome-3-4/ui.tar.gz" -O gnome-3-4.ui.zh_CN.`date +%F`.tar.gz
wget "http://l10n.gnome.org/languages/zh_CN/gnome-extras/ui.tar.gz" -O gnome-extras.ui.zh_CN.`date +%F`.tar.gz
wget "http://l10n.gnome.org/languages/zh_CN/external-deps/ui.tar.gz" -O external-deps.ui.zh_CN.`date +%F`.tar.gz
wget "http://l10n.gnome.org/languages/zh_CN/gnome-gimp/ui.tar.gz" -O gnome-gimp.ui.zh_CN.`date +%F`.tar.gz
wget "http://l10n.gnome.org/languages/zh_CN/gnome-infrastructure/ui.tar.gz" -O gnome-infrastructure.ui.zh_CN.`date +%F`.tar.gz
wget "http://l10n.gnome.org/languages/zh_CN/gnome-office/ui.tar.gz" -O gnome-office.ui.zh_CN.`date +%F`.tar.gz
