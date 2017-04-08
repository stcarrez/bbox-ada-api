# API Ada pour la Bbox de Bouygues Telecom

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Druss.svg)](http://jenkins.vacs.fr/job/Druss/)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)

The **bbox-ada-api** provides an Ada API to access the [Bouygues Telecom Bbox](https://www.bouyguestelecom.fr/offres-internet/bbox).
The Bbox gateway API is documented at https://api.bbox.fr/doc/apirouter/.
The repository also provides **Druss**, a command line tool that uses the API to perform some useful
actions on the Bbox.  

This repository being probably most useful to French users, the rest of this document will be in French.

# Build

Avant de compiler il vous faudra installer la librarie Ada suivante:

* Ada Util (https://github.com/stcarrez/ada-util)

Ensuite, lancer la commande **configure** et **make**:

```
  ./configure
  make
```

# Druss

Druss utilise l'API de la Bbox pour récupérer ses états et la controller.
Druss propose plusieurs commandes qu'il est possible de lancer en ligne de commande
ou dans un mode interactif.  Druss propose aussi une commande générique
permettant d'intéroger n'importe quelle API de la Bbox.  Druss a un mode
intéractive que vous pouvez activer avec l'option *-i*.

## Commandes bbox

La première commande a utiliser est la commande **druss bbox discover** pour demander
à Druss de découvrir les Bbox connectées.  Techniquement la découverte est basée sur
le protocole SSDP et la découverte du service UPnP/igd de la Bbox.  Druss est capable
de gérer plusieurs Bbox sur le réseaux (plutot rare).

```
$ druss bbox discover
```

Il est ensuite nécessaire d'indiquer le mot de passe permettant à Druss d'accéder
aux APIs sécurisées de la Bbox.

```
$ druss bbox password <passwd>
```

## Commande status

La commande de status indique des informations générales sur la Bbox et la connexion
Internet.  Dans la liste ci-dessous, deux Bbox sont indiquées, une première avec l'adresse
IP *192.168.0.1* et une seconde avec *192.168.1.240*.  Il y a de grande chances pour que
la commande n'en reporte qu'une et avec l'adresse IP *192.168.1.254*.
```
$ druss status
LAN IP          WAN IP          Internet VoIP  Wifi 2.4G Wifi 5G   Parental  DynDNS    Devices     Uptime
192.168.0.1     X.XX.XX.XXX     OK       Up    ON        ON        2                   18          24d 9h 23m
192.168.1.240   XXX.XXX.XXX.XX  OK       Up    ON        ON        0                   14          7d 41m
```

## Commande devices

La commande **druss devices** permet de lister les équipements qui ont été détectés par la Bbox.
Par défaut la commande ne va lister que les équipements actifs mais il est possible d'obtenir
la liste de tous les équipements.
```
$ druss devices
Bbox IP         Device IP       Ethernet            Hostname                    Type  Link
192.168.0.1     192.168.0.37    XX:XX:XX:XX:XX:XX   Bbox-TV-001                 STB   Ethernet port 1
192.168.0.1     192.168.0.90    XX:XX:XX:XX:XX:XX   Babybel                     STB   Ethernet port 1
192.168.0.1     192.168.0.17    XX:XX:XX:XX:XX:XX   zebulon                           Ethernet port 1
192.168.0.1     192.168.0.58    XX:XX:XX:XX:XX:XX   ouranus                           Wifi 5 RSSI -64
192.168.0.1     192.168.0.59    XX:XX:XX:XX:XX:XX   android-stephane                  Wifi 2.4 RSSI -70
192.168.0.1     192.168.0.72    XX:XX:XX:XX:XX:XX   Hestia                            Wifi 5 RSSI -78
192.168.0.1     192.168.0.6     XX:XX:XX:XX:XX:XX   pollux                            Ethernet port 2
192.168.0.1     192.168.0.67    XX:XX:XX:XX:XX:XX   android-2c4cb4ebaad79fda          Wifi 2.4 RSSI -60
192.168.1.240   192.168.1.129   XX:XX:XX:XX:XX:XX   zebulon                           Ethernet port 3
```

## Commande ping

La command **druss ping** demande à la Bbox de lancer un **ping** sur chacuns des équipements
et ensuite de donner la liste des équipements connectés avec le temps du ping.
```
$ druss ping
Bbox IP         Device IP       Hostname                    Ping           Link
192.168.0.1     192.168.0.37    Bbox-TV-001                         884 us Ethernet port 1
192.168.0.1     192.168.0.90    Babybel                           1.115 ms Ethernet port 1
192.168.0.1     192.168.0.17    zebulon                           1.434 ms Ethernet port 1
192.168.0.1     192.168.0.56    android-65b0948ba68c8618        881.215 ms Wifi 5 RSSI -57
192.168.0.1     192.168.0.58    ouranus                          16.099 ms Wifi 5 RSSI -65
192.168.0.1     192.168.0.59    android-stephane                156.258 ms Wifi 2.4 RSSI -69
192.168.0.1     192.168.0.72    Hestia                             1.063 s Wifi 5 RSSI -59
192.168.0.1     192.168.0.6     pollux                              671 us Ethernet port 2
192.168.0.1     192.168.0.67    android-2c4cb4ebaad79fda        534.474 ms Wifi 2.4 RSSI -56
192.168.1.240   192.168.1.129   zebulon                             740 us Ethernet port 3
```

## Commandes wifi

La commande Wifi permet d'obtenir des informations sur le Wifi, de l'arreter ou le démarrer.
Le status du Wifi est obtenu par la commande **druss wifi**.  La clef d'accès Wifi n'est
pas reportée par la commande.
```
$ druss wifi
Bbox IP        Enable  Channel SSID                Protocol    Encryption  Devices
192.168.0.1    ON      11      icare-home-wireless WPA+WPA2    AES         18
192.168.0.1    ON      40      Aragorn-5GHz        WPA+WPA2    AES
192.168.1.240  ON      8       Gandalf-Le-Blanc    WPA+WPA2    AES         14
192.168.1.240  ON      132     Sauron              WPA+WPA2    AES
```

Pour allumer ou éteindre le Wifi, il faut utiliser la commande **druss wifi on** ou **druss wifi off**.
Cette commande va jouer sur l'activation de la radio Wifi de la Bbox.  Il est possible
de faire son propre planificateur Wifi ou bien de réactiver le Wifi lorsque le planificateur
Wifi de la Bbox l'a éteint.
```
$ druss wifi off
```

## Commande get

La command **druss get** permet d'executer n'importe quelle API **GET** de l'API Bbox.
Le résultat est ensuite retourné au format JSON.  Le résultat de la commande est un JSON
valide qui peut ensuite être analysé, reformatté (par **jshon** par exemple).

```
$ druss get device | jshon
[
 {
  "device": {
   "now": "2017-04-08T21:40:37Z",
   "status": 1,
   "numberofboots": 37,
   "modelname": "TVW620.I",
   "user_configured": 1,
   "serialnumber": "XXXXX",
   "display": {
    "luminosity": 100,
    "state": "."
   },
   "main": {
    "version": "11.1.4",
    "date": "2016-07-11T15:34:49Z"
   },
   "reco": {
    "version": "11.1.4",
    "date": "2016-07-11T15:27:24Z"
   },
   "bcck": {
    "version": "8.5.16"
   },
   "ldr1": {
    "version": "8.5.16"
   },
   "ldr2": {
    "version": "8.5.16"
   },
   "firstusedate": "2014-10-01T15:51:44Z",
   "uptime": 2108913
  }
 },
 {
  "device": {
   "now": "2017-04-08T21:40:39+0200",
   "status": 1,
   "numberofboots": 46,
   "modelname": "F@st5330b",
   "user_configured": 1,
   "serialnumber": "XXXXXX",
   "display": {
    "luminosity": 100,
    "state": "."
   },
   "main": {
    "version": "12.0.20",
    "date": "2017-03-09T13:51:00Z"
   },
   "reco": {
    "version": "12.0.20",
    "date": "2017-03-09T13:42:37Z"
   },
   "running": {
    "version": "12.0.20",
    "date": "2017-03-09T13:49:52+0100"
   },
   "bcck": {
    "version": "8.8.18"
   },
   "ldr1": {
    "version": "8.8.18"
   },
   "ldr2": {
    "version": "8.8.18"
   },
   "firstusedate": "",
   "uptime": 608779,
   "using": {
    "ipv4": 1,
    "ipv6": 0,
    "ftth": 0,
    "adsl": 1,
    "vdsl": 0
   }
  }
 }
]
```
