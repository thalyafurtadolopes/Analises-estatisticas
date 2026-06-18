@echo off
chcp 65001 > nul
title FIP 606 - Analise Estatistica
echo =======================================================
echo    Iniciando o Aplicativo de Analise Estatistica...
echo =======================================================
echo.
echo Procurando a instalacao do R no seu computador...

set "RSCRIPT="
for /d %%d in ("C:\Program Files\R\R-*") do (
    if exist "%%d\bin\Rscript.exe" set "RSCRIPT=%%d\bin\Rscript.exe"
)

if not defined RSCRIPT (
    echo [ERRO] O R nao foi encontrado na pasta padrao ^(C:\Program Files\R^).
    echo [AJUDA] Certifique-se de que o R esta instalado no seu computador.
    pause
    exit /b
)

echo R encontrado! Iniciando o servidor local...
echo O seu navegador ira abrir automaticamente em instantes.
echo AVISO: Nao feche esta janela preta enquanto estiver usando o aplicativo!
echo.

"%RSCRIPT%" -e "shiny::runApp('%~dp0myapp\app.R', launch.browser=TRUE)"

