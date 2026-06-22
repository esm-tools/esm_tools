IMPORTANT (last update 2025-09-26)
With foci versions
- default_mct5
- moz
CONSERV coupling instead of GAUSWGT coupling is used.
This change has been introduced as I (Sebastian Wahl) saw patchy patterns 
in the Arctic and a strong cold bias in the Weddel Sea with GAUSWGT coupling settings
when configuring foci-moz for the SOLVe project (Wenjuan Huo)
GAUSWGT was recommended by Eric Maissonave in 202x, since the COSERV coupling we used
in FOCI 1.x didn't work with MCT4 when we upgraded from MCT 2.8 used in FOCI1.x 
Joakim Kjellsson also had problems with GAUSWGT but switched to BILINEAR and BICUBIC in 
FOCIOIFS 4.0 instead.
Strange enough that no one noticed this during the last two years.

Sebastian Wahl 2025-09-26
