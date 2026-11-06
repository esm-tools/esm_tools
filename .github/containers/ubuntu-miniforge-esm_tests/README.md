To build the container from Mac OS:

```bash
docker buildx build --platform linux/x86-64 -t ubuntu-miniforge-esm_tests .
```

To run and test the container:

```bash
docker run -it --rm ubuntu-miniforge-esm_tests
```

To tag it and push it to Docker Hub:

```bash
docker tag ubuntu-miniforge-esm_tests mandresm/ubuntu-miniforge-esm_tests:v2.0
docker login
docker push mandresm/ubuntu-miniforge-esm_tests:v2.0
```

Versions:
- `v1.0`: Ubuntu 25.04, Python 3.9 (esm_tools < Python 3.10 support drop)
- `v2.0`: Ubuntu 24.04, Python 3.12

A new version needs a new tag (and the `image:` of the workflows updated in the same
pull-request), so that older branches keep using the image they were tested with:

```bash
grep -rn "ubuntu-miniforge-esm_tests:" .github/workflows
```
