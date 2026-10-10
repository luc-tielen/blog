# Blog

This repository contains the Astro source for my personal blog, available at
[luctielen.com](https://luctielen.com).

## Development

```sh
npm ci
npm run dev
```

## Production build

```sh
npm run build
```

The static site is written to `dist/`. Pushes to `master` are built and deployed
to GitHub Pages by `.github/workflows/deploy-pages.yml`.
