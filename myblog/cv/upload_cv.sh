#!/bin/bash
# A script that pushes my csv and updates my files
CWD=$(pwd)

cd ~/OneDrive/Quarto

quarto render cv.qmd --to pdf

#using version 2 of my resume now
mv cv.pdf resume.pdf

cp resume.pdf ~/Library/CloudStorage/OneDrive-Personal/Quarto/personal_quarto_site/files

cp resume.pdf ~/Documents/Application_Materials/

cp resume.pdf ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/Documents/

cp resume.pdf ~/Library/CloudStorage/OneDrive-Personal/Documents/Applications/

quarto render ~/Library/CloudStorage/OneDrive-Personal/Quarto/personal_quarto_site

cd ~/Library/CloudStorage/OneDrive-Personal/Quarto/personal_quarto_site

git pull

git add .

git commit -m 'update resume '$(date +%d.%m.%y-%H:%M:%S)

git push

cd ~/OneDrive/Quarto/CV_Quarto

git pull

git add .

git commit -m 'update resume '$(date +%d.%m.%y-%H:%M:%S)

git push

cd $CWD
