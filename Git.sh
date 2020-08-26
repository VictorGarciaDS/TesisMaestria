#!/bin/bash
com="'Actualización automática del "
currentDate=`date`
#git pull
#chmod 777 main.tex
git add main.tex MibibliotecaC.bib CargaDatos.R FuncionesLlenado.R NoParametrica.R Superficies.R
#git commit -m $com$currentDate$"'"
git commit -m 'Actualización automática'
git push
