import { getCollection } from 'astro:content';
import { experimental_AstroContainer as AstroContainer } from 'astro/container';

export async function GET() {
  const posts = await getCollection('posts');
  const container = await AstroContainer.create();

  const sortedPosts = posts.sort(
    (a, b) => parseDate(b.data.postDate).getTime() - parseDate(a.data.postDate).getTime()
  );

  const site = 'https://luctielen.com';
  const now = new Date().toISOString();

  const entries = await Promise.all(
    sortedPosts.map(async (post) => {
      // Preserve the stable URLs and midnight-UTC timestamps used by the old feed.
      const url = `${site}/posts/${post.slug}`;
      const date = parseDate(post.data.postDate).toISOString().replace('.000Z', 'Z');
      const { Content } = await post.render();
      const content = await container.renderToString(Content);
      const categories = post.data.tags
        .map((tag) => `    <category term="${escapeXml(tag)}" />`)
        .join('\n');
      const summary = post.data.description
        ? `    <summary>${escapeXml(post.data.description)}</summary>\n`
        : '';

      return `  <entry>
    <title>${escapeXml(post.data.title)}</title>
    <link href="${url}" />
    <id>${url}</id>
    <updated>${date}</updated>
    <author><name>${escapeXml(post.data.author)}</name></author>
${categories ? `${categories}\n` : ''}${summary}    <content type="html"><![CDATA[${escapeCdata(content)}]]></content>
  </entry>`;
    })
  );

  const xml = `<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>FP -&gt; Compilers -&gt; Logic -&gt; Blog</title>
  <link href="${site}/atom.xml" rel="self" type="application/atom+xml" />
  <link href="${site}/" />
  <id>${site}/</id>
  <updated>${now}</updated>
  <author><name>Luc Tielen</name></author>
${entries.join('\n')}
</feed>`;

  return new Response(xml, {
    headers: { 'Content-Type': 'application/atom+xml; charset=utf-8' },
  });
}

function parseDate(str: string): Date {
  const match = /^(?<month>[A-Z][a-z]{2}) (?<day>\d{1,2}), (?<year>\d{4})$/.exec(str);
  if (!match?.groups) {
    throw new Error(`Unsupported post date: ${str}`);
  }

  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  const month = months.indexOf(match.groups.month);
  if (month === -1) {
    throw new Error(`Unsupported post month: ${match.groups.month}`);
  }

  return new Date(Date.UTC(Number(match.groups.year), month, Number(match.groups.day)));
}

function escapeXml(str: string): string {
  return str
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}

function escapeCdata(str: string): string {
  return str.replace(/]]>/g, ']]]]><![CDATA[>');
}
