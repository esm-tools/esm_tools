# esm_tools issue 1147

- https://github.com/esm-tools/esm_tools/issues/1147
- softlinks are ignored by venv depending on python version
- workaround: use copy instead of softlink:
```
# remove links
rm ssp[0-9]*
# copy
for ssp in 119 126 245 370 534os 585; do 
    echo $ssp
    cp -r ssp ssp${ssp}
done
```

