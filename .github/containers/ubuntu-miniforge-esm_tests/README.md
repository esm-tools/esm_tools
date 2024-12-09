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
docker tag ubuntu-miniforge-esm_tests mandresm/ubuntu-miniforge-esm_tests:v1.0
docker login
docker push mandresm/ubuntu-miniforge-esm_tests:v1.0
```
