import { defineCollection, z } from 'astro:content';

const posts = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    author: z.string(),
    postDate: z.string(),
    tags: z.array(z.string()).optional().default([]),
    image: z.string().optional(),
    description: z.string().optional(),
  }),
});

const videos = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    author: z.string(),
    videoDate: z.string(),
    youtubeUrl: z.string(),
    description: z.string().optional(),
  }),
});

export const collections = { posts, videos };
