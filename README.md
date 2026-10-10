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

The static site is written to `dist/`.

## Deployment

The production site remains hosted by Netlify behind Cloudflare. Netlify watches
the `deploy` branch and publishes its `build/` directory. To build the Astro site
and update that branch, run:

```sh
./deploy.sh
```
