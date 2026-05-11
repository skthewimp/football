# Cleaning Up an Old Football Folder Into a Real Project

This started from a familiar problem: an old folder with some useful code, a lot of forgotten analysis, and just enough data-fetching logic to make you wonder whether any of it still works.

The useful core turned out to be a ClubElo pipeline for English football clubs. The old scripts were pulling historical Elo timelines for clubs, joining them to manager tenures, and then visualizing how a club's rating moved from one managerial regime to the next. That part was still worth saving. The rest of the folder was a decade of side quests: notebooks, PDFs, plots, world cup experiments, Opta files, and a giant local SQLite database.

So the cleanup was not "make every old file pretty." It was "decide what this project actually is." I narrowed it to one maintainable repo:

- fetch English club histories from ClubElo
- scrape current manager-list pages from Wikipedia
- rebuild the joined manager/Elo dataset
- expose the result through a small Shiny app

The most useful technical decision was to stop pretending the old folder had one coherent structure. Instead of preserving the sprawl, I split it into a maintained project and a clearly labeled archive. That keeps the live code easy to run while still preserving the old work for reference.

The other interesting part was source fragility. Old sports-analysis code often "works" only because a website looked a certain way years ago. Rebuilding the pipeline meant testing whether ClubElo still exposed the club-history CSV endpoints and making the manager-table scraping less dependent on exact table positions.

If you’ve accumulated a similar graveyard of half-remembered sports scripts, the lesson is simple: don’t document the chaos, reduce it. Pick the one artifact worth maintaining, build a clean path to reproduce it, and archive the rest honestly.

Repo: https://github.com/skthewimp/football
