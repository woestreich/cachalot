# cachalot

This project identifies and analyzes cachalot (sperm whale; Physeter macrocephalus) echolocation clicks found in 7 years of near continuous recording from the Monterey Bay Aquarium Research Institute (MBARI) MARS hydrophone. 

Full hydrophone data archive: https://www.mbari.org/data/passive-acoustic-data/

MBARI Ocean Soundscape Team: https://www.mbari.org/team/ocean-soundscape/

Energy detection files (data/BLED) are daily tables of potential cachalot click detections. These tables are then processed to identify true positive sperm whale click sequences using inter-click-intervals of a constant, repetitive nature (code/1_processing/daynight_presence.R). This methodology has been assessed for performance and optimized for key parameters (code/O_performance_assess). Further processing includes calculation of sequences of consecutive recording days with click presence (code/1_processing/sequences.R). These processed results are analyzed and visualized (outputs/figures) using the script "code/2_analysis/figures.R".

BLED detections were generated in Raven Pro v.1.6.3 (Cornell Lab of Ornithology), using a Band Limited Energy Detector with the following parameters:

BLED signal calculation:
Min. Frequency	= 1.4 kHz
Max. Frequency	= 4.0 kHz
Min. Duration	= 8.125 ms
Max. Duration	= 32.5 ms
Min. Separation	= 32.5 ms

BLED noise calculation:
Block size	= 2.0 s
Hop size	= 0.5 s
Percentile	= 20.0

Signal-to-noise parameters:
Min. Occupancy	= 70.0%
SNR Threshold	= 5.0 dB

Spectrogram calculation:
Window		= Hann
Window Size	= 512 samples
Window Overlap	= 95%
