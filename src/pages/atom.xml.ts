import { getCollection } from 'astro:content';

export async function GET() {
  const posts = await getCollection('posts');

  function parseDate(str: string): Date {
    return new Date(str);
  }

  const sortedPosts = posts.sort(
    (a, b) => parseDate(b.data.postDate).getTime() - parseDate(a.data.postDate).getTime()
  );

  const site = 'https://luctielen.com';
  const now = new Date().toISOString();

  const entries = sortedPosts
    .map((post) => {
      const url = `${site}/posts/${post.slug}/`;
      const date = parseDate(post.data.postDate).toISOString();
      return `
  <entry>
    <title>${escapeXml(post.data.title)}</title>
    <link href="${url}" />
    <id>${url}</id>
    <updated>${date}</updated>
    <author><name>Luc Tielen</name></author>
  </entry>`;
    })
    .join('');

  const xml = `<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>FP -&gt; Compilers -&gt; Logic -&gt; Blog</title>
  <link href="${site}/atom.xml" rel="self" />
  <link href="${site}" />
  <id>${site}/atom.xml</id>
  <updated>${now}</updated>
  <author><name>Luc Tielen</name></author>
${entries}
</feed>`;

  return new Response(xml, {
    headers: { 'Content-Type': 'application/atom+xml; charset=utf-8' },
  });
}

function escapeXml(str: string): string {
  return str
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}
