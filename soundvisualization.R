# Visualizaciones ad hoc de sonido con R
# www.datosimagensonido.com

library(seewave)  # 'tico'
library(tuneR)  # plot()
library(phonTools)  # spectrogram()
library(tiff)

data(tico)  # Cargamos la forma de onda 'tico'
listen(tico)  # Escuchamos al pajarillo
str(tico)  # Estructura de la clase 'Wave': longitud, canales, frec. muestreo,...


# VISUALIZACIONES EST�NDAR

# Visualizaci�n est�ndar del oscilograma
plot(tico)
plot(tico@left, type='l')

# Visualizaci�n est�ndar del espectrograma
spectrogram(tico@left, fs=tico@samp.rate, maxfreq=10000,
    windowlength=15, timestep=1, preemphasisf=50000)



# VISUALIZACIONES AD HOC


# Visualizaci�n ad hoc del oscilograma
soundwave=tico@left
N=77  # Muestras de forma de onda por valor final mostrado
soundwave=soundwave[1:(floor(length(soundwave)/N)*N)]
soundwave=floor(soundwave/32768*128)  # Rango 16 bits -> 8 bits

L=length(soundwave)  # L siempre es m�ltiplo entero de N
soundbmp=array(0, c(L/N, 256))  # Rango -128..0..127

for (i in 1:(L/N)) {
  for (j in 1:N) {
    valor=soundwave[(i-1)*N+j]
    for (n in 0:valor) soundbmp[i,n+128+1]=soundbmp[i,n+128+1] + 1
  }
}

writeTIFF(soundbmp/max(soundbmp), "soundwave.tif",
    bits.per.sample=16, compression="LZW")


# Visualizaci�n ad hoc del espectrograma
# Leemos como imagen espectrograma monocromo (colors=F) de spectrogram()
espectro=readTIFF("fft.tif", native=F, convert=F)

TIEMPO=nrow(espectro)
FRECUENCIA=ncol(espectro)
SEPARACION=5  # Separaci�n de las l�neas
ALTURA=117  # Altura en p�xeles de los m�ximos

espectrograma=array(0, c(TIEMPO*SEPARACION, FRECUENCIA))

for (t in 1:TIEMPO) {
  for (frec in 1:FRECUENCIA) {
    for (i in 0:round(espectro[t,frec]*ALTURA))
      espectrograma[t*SEPARACION-i, frec]=i/ALTURA
    espectrograma[t*SEPARACION-espectro[t,frec]*ALTURA, frec]=0.5
  }
}

writeTIFF(espectrograma, "spectrogram.tif",
    bits.per.sample=16, compression="LZW")