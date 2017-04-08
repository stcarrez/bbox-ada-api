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
permettant d'intéroger n'importe quelle API de la Bbox.



