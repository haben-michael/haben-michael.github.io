#!/usr/bin/env python3
"""
Build index.html from a BibTeX file and an HTML template.

Usage:
    python build_index.py publications.bib index_template.html index.html

- publications.bib       Your .bib file (with keywords = {haben,...}).
- index_template.html    Template with an empty <div><ol>...</ol></div> for Research.
- index.html             Output file to write.
"""

import argparse
import html
import re
from pybtex.database import parse_file

# Simple priority for "status" keywords, if present
STATUS_ORDER = {
    "published": 0,
    "in-press": 1,
    "submitted": 2,
    "preprint": 3,
}


def clean_tex(s: str) -> str:
    """Very lightweight TeX → plain text cleanup for titles, notes, etc."""
    if not s:
        return ""
    # Normalize whitespace
    s = re.sub(r"\s+", " ", s)
    # Replace common TeX-ish bits
    s = s.replace(r"\&", "&")
    # Drop protective braces
    s = s.replace("{", "").replace("}", "")
    return s.strip()


def format_person(person) -> str:
    """Format a pybtex Person as 'First Middle Last'."""
    first = " ".join(person.first_names + person.middle_names)
    last = " ".join(person.prelast_names + person.last_names + person.lineage_names)
    if first and last:
        return f"{first} {last}"
    return first or last


def format_authors(entry) -> str:
    persons = entry.persons.get("author", [])
    names = [clean_tex(format_person(p)) for p in persons]
    if not names:
        return ""
    if len(names) == 1:
        return names[0]
    if len(names) == 2:
        return f"{names[0]} and {names[1]}"
    return ", ".join(names[:-1]) + " and " + names[-1]


def build_citation(entry) -> str:
    """Build the journal/venue/year part of the citation."""
    f = entry.fields
    year = f.get("year")
    note = clean_tex(f.get("note", "")) or None
    parts = []

    if entry.type == "article":
        journal = clean_tex(f.get("journal"))
        volume = clean_tex(f.get("volume"))
        number = clean_tex(f.get("number"))
        pages = clean_tex(f.get("pages"))

        if journal:
            piece = journal
            if volume:
                piece += f", {volume}"
                if number:
                    piece += f"({number})"
            if pages:
                piece += f" {pages}"
            parts.append(piece)

        if note:
            parts.append(note)

    elif entry.type == "techreport":
        if note:
            parts.append(note)
        else:
            inst = clean_tex(f.get("institution"))
            if inst:
                parts.append(f"Technical Report, {inst}")

    else:  # unpublished / other
        if note:
            parts.append(note)

    if year:
        parts.append(year)

    return ", ".join(parts)


def extract_status(entry):
    """Look for one of our status keywords in the 'keywords' field."""
    kws = [
        k.strip().lower()
        for k in entry.fields.get("keywords", "").split(",")
        if k.strip()
    ]
    for k in STATUS_ORDER:
        if k in kws:
            return k
    return None


def entry_sort_key(entry):
    """Sort by year (desc), then status priority, then title."""
    year_str = entry.fields.get("year", "0")
    try:
        year_match = re.findall(r"\d{4}", year_str)
        year = int(year_match[0]) if year_match else 0
    except Exception:
        year = 0

    status = extract_status(entry) or "zzz"
    status_rank = STATUS_ORDER.get(status, 99)
    title = clean_tex(entry.fields.get("title", ""))

    # Newest year first, then more "complete" status (published first)
    return (-year, status_rank, title)


def build_links(entry) -> str:
    """Create HTML <a> links from link_* fields in a consistent order."""
    f = entry.fields
    link_fields = [
        ("link_slides", "Slides"),
        ("link_poster", "Poster"),
        ("link_preprint", "Preprint"),
        ("link_journal", "Journal"),
        ("link_code", "Code"),
    ]
    links = []
    for field, label in link_fields:
        url = f.get(field)
        if url:
            links.append(f'<a href="{html.escape(url)}">{label}</a>')
    return ", ".join(links)


def format_entry_html(entry) -> str:
    """Turn a BibTeX entry into one <li>…</li></br>."""
    authors = format_authors(entry)
    title = clean_tex(entry.fields.get("title", ""))
    citation = clean_tex(build_citation(entry))

    pieces = []
    if authors:
        pieces.append(f"{authors}.")
    if title:
        pieces.append(f"{title}.")
    if citation:
        pieces.append(f"{citation}.")

    main_text = " ".join(pieces)
    links_html = build_links(entry)

    safe_main = html.escape(main_text)
    if links_html:
        return f"<li>{safe_main} {links_html}.</li></br>"
    else:
        return f"<li>{safe_main}</li></br>"


def insert_publications(template_html: str, items_html: str) -> str:
    """
    Replace the empty <div><ol>...</ol></div> block with the generated items.

    Expects the template to contain exactly one block like:

        <div><ol>
        </ol></div>
    """
    marker_start = "<div><ol>"
    marker_end = "</ol></div>"

    start = template_html.find(marker_start)
    end = template_html.find(marker_end, start)
    if start == -1 or end == -1:
        raise RuntimeError('Could not find "<div><ol> ... </ol></div>" block in template.')

    before = template_html[:start]
    after = template_html[end + len(marker_end) :]
    return before + marker_start + "\n" + items_html + "\n" + marker_end + after


def main():
    parser = argparse.ArgumentParser(
        description="Build index.html from BibTeX and HTML template."
    )
    parser.add_argument("bibfile", help="Input BibTeX file")
    parser.add_argument(
        "template",
        help="HTML template file (with empty <div><ol>...</ol></div> for Research)",
    )
    parser.add_argument("output", help="Output HTML file (index.html)")
    args = parser.parse_args()

    # Parse BibTeX
    bib_data = parse_file(args.bibfile)

    # Keep only entries tagged with 'haben' in keywords
    entries = []
    for key, entry in bib_data.entries.items():
        kws = [
            k.strip().lower()
            for k in entry.fields.get("keywords", "").split(",")
            if k.strip()
        ]
        if "haben" in kws:
            entries.append(entry)

    # Sort entries
    entries.sort(key=entry_sort_key)

    # Build HTML list items
    items_html = "\n".join(format_entry_html(e) for e in entries)

    # Read template
    with open(args.template, encoding="utf-8") as f:
        template_html = f.read()

    # Insert publications
    final_html = insert_publications(template_html, items_html)

    # Write output
    with open(args.output, "w", encoding="utf-8") as f:
        f.write(final_html)


if __name__ == "__main__":
    main()
