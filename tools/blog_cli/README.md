tazblog CLI
===========

My blog stores its content in DNS, spread out over three types of `TXT` entries:

* `TXT _posts.blog.tazj.in.`: A sorted list of posts, serialised as a JSON list of
  strings (e.g. `["1486830338", "1476807384"]`)

* `TXT _chunks.$postID.blog.tazj.in`: JSON chunks containing the blog post text

* `TXT _meta.$postID.blog.tazj.in`: JSON blob with blog post metadata

All JSON blobs are base64-encoded.

This CLI tool helps to update those records.

Each blog post data is a series of JSON-encoded structures which follow one of
these formats:

```
struct metadata {
    chunks: int
    title: string
    date: date
}
```

Where `chunks` describes the number of chunks following this format:

```
struct chunk {
    c: int
    t: string
}
```

Writing a blog post to DNS means taking its text and metadata, chunking it up
and writing the chunks.

Reading a blog post means retrieving all data, reading the metadata and then
assembling the chunks in order.
