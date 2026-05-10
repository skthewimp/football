# Dev Log

## 2026-05-10

User prompt:
> there is some old code here about getting elo ratings from some website for differnet clubs, and then doing a bunch of analysis wiht that. can you update the data on that (hopefully it still exists)? and before that, can you clean up this folder and push it to github wiht adequate documentations?

What changed:

- Audited the loose folder and identified the ClubElo-related subset.
- Reorganized the project into `scripts/`, `data/`, `app/`, `legacy/`, and `archive/local-only/`.
- Moved the old ClubElo scripts into `legacy/clubelo/`.
- Moved unrelated notebooks, plots, PDFs, and old analysis into `legacy/misc/`.
- Moved large local-only artifacts such as `database.sqlite` and R session files into `archive/local-only/`.
- Replaced the old refresh code with reproducible scripts for:
  - downloading English ClubElo histories
  - rebuilding the manager/Elo join from live Wikipedia tables
  - regenerating compact outputs in `data/`
- Rebuilt the Shiny app so it reads from the new layout.
- Added repo documentation for setup, refresh, and scope.

Key issues hit:

- The folder was not a git repository.
- The original scripts depended on mutable working directories and hard-coded paths.
- The original refresh logic bootstrapped club names from stale `RData` files and assumed old API behavior.
- Network access from the sandbox required explicit approval for live checks.
- A `299MB` SQLite file in the root would have made a direct GitHub push fail.

Decisions rejected:

- Keeping the entire folder as a single repo without narrowing scope. That would have produced a noisy and brittle repository.
- Rewriting every historical notebook. The maintained project is now the ClubElo pipeline and app; the rest is archived, not polished.

Next steps:

- Run the live refresh end to end and verify the current output shape.
- Initialize git, commit the cleaned project, and push to GitHub once the remote target is set.
