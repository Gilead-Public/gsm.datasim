# AGENTS.md

## Overview

This file sets the agent’s context for work in the **gsm ecosystem**:
what the repos are (below), how to behave across them (Conventions), and
what tools to reach for (Skills). It is intended for **general use** —
it applies to any gsm task.

Task-specific procedures (like the Red-Green TDD issue → PR lifecycle)
are *not* baked into this always-loaded file. They live as **skills**
under [`skills/`](https://gilead-public.github.io/gsm.datasim/skills/) —
discrete capabilities the agent reaches for when a task matches the
skill’s description, loading the full procedure only then. See
[Skills](#skills) below. The model is: **conventions** are the always-on
guardrails; **skills** are the tools the agent picks up as needed.

## Table of Contents

- [The gsm Ecosystem](#the-gsm-ecosystem)
- [Conventions](#conventions)
  - [Issue, PR, and Comment
    Attribution](#issue-pr-and-comment-attribution)
  - [GitHub Link Sharing Convention](#github-link-sharing-convention)
  - [Draft File Convention](#draft-file-convention)
  - [Draft Status Header Convention](#draft-status-header-convention)
  - [Draft Sync Convention](#draft-sync-convention)
  - [GitHub Properties for Draft
    Items](#github-properties-for-draft-items)
  - [Assignee Convention](#assignee-convention)
  - [Issue–PR Link Convention](#issuepr-link-convention)
  - [GitHub Templates Convention](#github-templates-convention)
  - [Approval Convention](#approval-convention)
  - [Decision Prompt Convention](#decision-prompt-convention)
  - [Test-Driven Development
    Convention](#test-driven-development-convention)
  - [Using Skills](#using-skills)
  - [Parallel Worktree Convention](#parallel-worktree-convention)
- [Skills](#skills)

------------------------------------------------------------------------

## The gsm Ecosystem

The agent works across ~40 independently-cloned repositories that make
up Gilead Biostatistics’ Risk-Based Quality Management (RBQM) /
**OpenRBQM** ecosystem. Most are R packages (hosted under the
`Gilead-BioStats` and `OpenRBQM` GitHub orgs); a few are JavaScript/Node
projects, static sites, or content libraries. Each repo is separate,
with its own remote, branches, and (sometimes) repo-local `AGENTS.md`.

> Each repo is its own checkout: `cd` into the specific repo before
> running git, R, or build commands. Repos are frequently on feature
> branches or have linked worktrees in sibling `*-worktrees/`
> directories — do not assume `dev`/`main`.

### Requirements source: gsm.roadmap

Ecosystem requirements originate in
[`gsm.roadmap`](https://github.com/Gilead-BioStats/gsm.roadmap) — the
planning repo that turns needs into scoped, ready-to-implement issues.
Its operations agent,
[`roadmap.agent.md`](https://github.com/Gilead-BioStats/gsm.roadmap/blob/main/.github/agents/roadmap.agent.md),
drafts those requirements and hands them off to the development agents
defined here. When a dev task needs the “why” behind an issue
(requirement context, parent linkage), consult the roadmap agent.

### The analytics pipeline (architectural core)

The original **`gsm`** (“Good Statistical Monitoring”) package is
**deprecated/unmaintained as of March 2025** and was split into modular
packages. New work happens in the modular packages, not `gsm`.

The pipeline assesses site-level risk in clinical-trial data via a
standardized **6-step data flow**:
`Input → Transform → Analyze → Threshold → Flag → Summarize`. Steps are
plain functions composed into **YAML workflows** (found in each
package’s `inst/workflow/`).

Dependency direction (all depend on `gsm.core`, which depends on none of
them):

- **`gsm.core`** — analytics engine + workflow-running utilities. The
  base package.
- **`gsm.mapping`** — maps raw/source datasets into standardized
  analysis domains.
- **`gsm.kri`** — Key Risk Indicators: metric generation +
  visualization/reporting.
- **`gsm.reporting`** — builds the reporting data model that feeds
  reports.
- **`gsm.qtl`** — Quality Tolerance Limits.
- **`gsm.simaerep`** — wraps the `simaerep` package as a KRI (AE
  under-reporting detection).

**`workr`** is the generalized workflow engine extracted from `gsm`’s
`RunWorkflow` functions — *Steps* (functions), *Meta* (config), *Spec*
(data spec), run via `RunStep()` / `RunWorkflow()` / `RunWorkflows()`
over YAML. **`open.gismo`** is an end-to-end open-source analytics
platform built on `workr`.

**`gsm.library`** is the central configuration repo: it aggregates the
YAML workflows and data specs from every package, plus pinned package
snapshots (`manifest.csv`, `rproject.toml`), with a non-standard branch
model (nightly `dev`/`main` snapshots → triggered `release-{name}` →
`prod`). Test data comes from the `clindata` package.

### Repo categories

- **Pipeline packages**: `gsm.core`, `gsm.mapping`, `gsm.kri`,
  `gsm.reporting`, `gsm.qtl`, `gsm.simaerep`, `grail` (Gilead Risk
  Signal and Action Listing).
- **Workflow / platform**: `workr`, `open.gismo`, `gsm.library`.
- **Data & simulation**: `gsm.datasim` (synthetic test data),
  `simaerep`, `ae.detector`.
- **Visualization (JS)**: `rbm-viz` (npm package name `gsm.viz`),
  `big.library`.
- **Dashboards / sites**: `gh.dash` (R pkg powering GitHub
  release/milestone dashboards), `gsm.dash`, `big.dash`, `gsm.site`,
  `gsm.guide`, `OpenRBQM.github.io`.
- **Specialty / utility**: `gsm.utils`, `gsm.template`,
  `gsm.study.profile`, `gsm.digitpref`, `gsm.roadmap`,
  `r-qualification`, `data.wg`, `playground`.

### Column conventions and data sources

Standard column identifiers used across the pipeline:

- **Subject ID**: `subjid`
- **Site / investigator**: `invid` (**not** `siteid`)
- **Study ID**: `studyid`
- **Group columns**: passed via the `strGroupCol` parameter in metric
  workflows

Bundled test/example data lives in `gsm.core` — use these when writing
tests or examples rather than synthesizing inputs from scratch:

- `gsm.core::lSource$Raw_SUBJ` — demographics
- `gsm.core::lSource$Raw_AE` — adverse events (plus other `Raw_*`
  domains in `lSource`)
- [`gsm.core::reportingResults`](https://gilead-public.github.io/gsm.core/reference/reportingResults.html)
  — pre-calculated KRI results
- [`gsm.core::reportingGroups`](https://gilead-public.github.io/gsm.core/reference/reportingGroups.html)
  — site / study metadata
- [`gsm.core::reportingMetrics`](https://gilead-public.github.io/gsm.core/reference/reportingMetrics.html)
  — metric definitions
- [`gsm.core::reportingBounds`](https://gilead-public.github.io/gsm.core/reference/reportingBounds.html)
  — statistical bounds

`gsm.datasim` is the source for additional synthetic test data when
richer scenarios are needed.

### Common commands

Most repos are R packages — run from inside the repo in R:

``` r

devtools::load_all()                              # load for interactive use
devtools::test()                                  # full testthat suite
devtools::test(filter = "foo")                    # only tests/testthat/test-foo.R
testthat::test_file("tests/testthat/test-foo.R")  # a single test file
devtools::document()                              # regen Rd + NAMESPACE from roxygen2
devtools::check()                                 # full R CMD check — must be clean
styler::style_pkg()                               # format before release
```

`devtools::check()` running clean (no errors/warnings/notes) is the gate
before an implementation commit (see the `tdd` skill). R packages use
**roxygen2** for docs, **testthat** for tests, and GitHub Actions for
CI. Node projects (`rbm-viz`, `big.library`) use npm — check each
`package.json` `scripts`.

------------------------------------------------------------------------

## Conventions

### Issue, PR, and Comment Attribution

- When creating or commenting on issues, pull requests, or GitHub
  discussions, include this attribution line at the top:
  - This {comment/Issue/PR} was drafted by {agent} using {model} and
    reviewed by {user}
- Use the specific agent and model name (for example:
  `GitHub Copilot using GPT-5.3-Codex`, or
  `Claude Code using Opus 4.8`).
- Place the attribution as the first line of the submitted text.

### Examples

Issue:

This Issue was drafted by Claude Code using Opus 4.8 and reviewed by
@jwildfire

PR:

This PR was drafted by Claude Code using Opus 4.8 and reviewed by
@jwildfire

Comment:

This comment was drafted by Claude Code using Opus 4.8 and reviewed by
@jwildfire

### GitHub Link Sharing Convention

- Whenever the agent posts an Issue, PR, or comment to GitHub, include
  the resulting URL in the next user-facing response.
- Present the URL as a clickable Markdown link so the user can open it
  directly.
- For multiple posted items, provide one clickable link per item (Issue,
  PR, comment), clearly labeled.
- If posting fails, explicitly state that no link is available yet and
  report the error summary.

### Draft File Convention

- Whenever drafting content, save it as a file named
  `{repo}_{type}_N_{briefdescription}.md`, in whatever location your
  harness uses for drafts or temporary files.
- Use `{repo}` as the repository name (for example: `workr`,
  `open.gismo`, `data.wg`).
- Use `{type}` as `PR` or `ISSUE`; if it is not a PR or ISSUE, use
  `DISCUSSION`.
- Use `{#}` as a sequential number per repo/type when posted; if it is
  not posted yet, use `N`.
- Use `{briefdescription}` as a concise slug under 20 characters.
- When posting a draft to GitHub, rename the file from
  `{repo}_{type}_N_` to `{repo}_{type}_{#}_` (replacing `N` with the
  actual number) and **delete the old `_N_` version** so only the posted
  copy remains.

#### Posting Checklist

Run these steps **in the same response** as the `gh issue create` /
`gh pr create` call that posted the draft — they are part of the posting
action, not optional follow-up:

1.  Record the number GitHub assigned (from the command output).
2.  Update the draft’s status header to
    `<!-- STATUS: Posted to {url} on {Date/Time} -->`.
3.  Rename the draft `{repo}_{type}_N_` → `{repo}_{type}_{#}_` and
    delete the old `_N_` version.
4.  Share the resulting URL as a clickable Markdown link (GitHub Link
    Sharing Convention).

Do not advance to the next phase (branching, writing tests, etc.) until
the checklist is complete.

### Draft Status Header Convention

- For files that are going to GitHub, add a status comment as the first
  line of the file.
- Use one of these exact formats:
  - `<!-- STATUS: Drafted on {Date/Time} -->`
  - `<!-- STATUS: Posted to {url} on {Date/Time} -->`

### Draft Sync Convention

Draft files and their posted GitHub counterparts drift apart unless kept
in sync. A draft is not write-once:

- **Re-read before referencing.** Before using a posted draft’s content
  — drafting a commit, responding to review, or summarizing a PR —
  re-read the live body with `gh pr view` / `gh issue view` rather than
  trusting the local file, which may be stale.
- **Update local when GitHub changes.** If the live body differs from
  the local draft, update the local `.md` to match and note the sync in
  your response.
- **Update GitHub when the draft changes.** If you revise a draft after
  it was posted (its status header reads `Posted to`), push the change
  with `gh pr edit --body-file` / `gh issue edit --body-file` and bump
  the status header timestamp.
- **Keep the PR description accurate.** When the implementation diverges
  from what a PR describes, update the description with
  `gh pr edit --body-file` so it always reflects the actual changes.

### GitHub Properties for Draft Items

- Add a comment section near the top of draft items (after STATUS and
  attribution) documenting any GitHub properties to set when posting.
- Include properties such as: `Milestone`, `Labels`, `Project`,
  `Assignee`, `Reviewers`, etc.
- Format as a comment block for easy reference during posting:
  - `<!-- GITHUB_PROPERTIES: Milestone: v1.0.0, Labels: enhancement, refactor -->`
- If no special properties are needed, omit this section.
- When posting, if a referenced property (e.g., Milestone, Label,
  Project) does not yet exist in the GitHub repo, **prompt the user** to
  ask whether it should be created before proceeding.

### Assignee Convention

- Always assign issues and PRs to `@me` when creating them.
- Pass `--assignee @me` to `gh issue create` and `gh pr create`.
- Include `Assignee: @me` in the `GITHUB_PROPERTIES` comment of every
  draft.

### Issue–PR Link Convention

- Every PR body must include a `Closes #X` line (or `Fixes #X` when
  appropriate) to auto-close the linked issue on merge.
- Place the closing keyword on its own line immediately after the
  opening summary paragraph.
- A PR may close multiple issues: list each on a separate `Closes #X`
  line.

### GitHub Templates Convention

- Before creating an issue or PR, check whether the repository has
  GitHub templates:
  - Issues: `.github/ISSUE_TEMPLATE/` directory or `ISSUE_TEMPLATE.md`
  - PRs: `.github/PULL_REQUEST_TEMPLATE.md` or
    `.github/PULL_REQUEST_TEMPLATE/`
- If templates exist, use `gh issue create --template <file>` or pass
  `--body-file` with the draft content **structured to match the
  template’s sections**.
- If no template exists, proceed without one — do not create templates
  unless explicitly asked.

#### Issue Creation with Template + Type

- Treat issue type as part of the template contract. Choose the template
  that maps to the intended type (for example, `3-feature.md` for
  `type: Feature`).
- Preferred: run `gh issue create --template <template-file>` from the
  target repository root so local template resolution works.
- If `gh --template` is unavailable or fails in the current CLI/session,
  use `--body-file` with draft content that exactly matches the selected
  template sections so type semantics are preserved.
- Always keep `Assignee: @me` and required labels when creating the
  issue.

Sample commands:

\# Preferred (template + type via frontmatter) cd /path/to/repo && gh
issue create –template 3-feature.md –title “

\<p\>” –assignee @me –label enhancement\</p\> \# Fallback
(template-structured body when –template is not available) gh issue
create -R owner/repo –title “ \<title\> \<p\>” –body-file
repo_ISSUE_N_topic.md –assignee @me –label enhancement\</p\> \</div\>
\<div id="example" class="section level4"\> \<h4\>Example\</h4\> \<div
class="sourceCode" id="cb2"\>\<pre class="sourceCode markdown"\>\<code
class="sourceCode markdown"\>\<span id="cb2-1"\>\<a href="#cb2-1"
tabindex="-1"\>\</a\>\<span class="co"\>\<!-- STATUS: Drafted on
2026-04-02 10:43:48 EDT --\>\</span\>\</span\> \<span id="cb2-2"\>\<a
href="#cb2-2" tabindex="-1"\>\</a\>\<span class="co"\>\<!--
GITHUB_PROPERTIES: Milestone: v1.0.0, Labels: critical, refactor,
Assignee: @me --\>\</span\>\</span\> \<span id="cb2-3"\>\<a
href="#cb2-3" tabindex="-1"\>\</a\>\</span\> \<span id="cb2-4"\>\<a
href="#cb2-4" tabindex="-1"\>\</a\>This Issue was drafted by Claude Code
using Opus 4.8 and reviewed by @jwildfire\</span\> \<span
id="cb2-5"\>\<a href="#cb2-5" tabindex="-1"\>\</a\>\</span\> \<span
id="cb2-6"\>\<a href="#cb2-6" tabindex="-1"\>\</a\>\<span class="fu"\>#
Issue Title\</span\>\</span\> \<span id="cb2-7"\>\<a href="#cb2-7"
tabindex="-1"\>\</a\>...\</span\>\</code\>\</pre\>\</div\> \</div\>
\</div\> \<div id="approval-convention" class="section level3"\>
\<h3\>Approval Convention\</h3\> \<ul\> \<li\>For user approval gates,
present a multiple-choice \<strong\>Approval\</strong\> prompt rather
than asking the user to type \<code\>approved\</code\>. Use whatever
interactive multiple-choice prompt the agent’s tool provides (for
example, \<code\>vscode_askQuestions\</code\> in VS Code, or
\<code\>AskUserQuestion\</code\> in Claude Code).\</li\> \<li\>Offer
options equivalent to: \<ul\> \<li\>\<code\>Approve and
continue\</code\>\</li\> \<li\>\<code\>Request changes\</code\>\</li\>
\<li\>\<code\>Pause\</code\>\</li\> \</ul\>\</li\> \<li\>Treat only an
explicit \<code\>Approve and continue\</code\> selection as approval to
proceed.\</li\> \<li\>If the user selects \<code\>Request
changes\</code\>, revise the draft and re-open the Approval
prompt.\</li\> \<li\>If the user selects \<code\>Pause\</code\>, stop
and wait for further direction.\</li\> \</ul\> \</div\> \<div
id="decision-prompt-convention" class="section level3"\> \<h3\>Decision
Prompt Convention\</h3\> \<ul\> \<li\>Whenever possible, present next
implementation steps as multiple-choice options rather than open-ended
prompts, using the agent’s interactive prompt.\</li\> \<li\>Keep
freeform input optional and only rely on open-ended text when options
are genuinely insufficient.\</li\> \<li\>If uncertainty remains after an
option is selected, follow up with a narrow, targeted question.\</li\>
\</ul\> \</div\> \<div id="test-driven-development-convention"
class="section level3"\> \<h3\>Test-Driven Development Convention\</h3\>
\<ul\> \<li\>For code changes — new functionality or bug fixes — default
to test-driven development: write or update tests first, confirm they
fail, then implement the minimal change to make them pass. The \<a
href="skills/tdd/SKILL.md"\>\<code\>tdd\</code\>\</a\> skill defines the
full Red-Green flow.\</li\> \<li\>This is a default to adapt, not an
absolute. Changes that can’t be meaningfully tested with
\<code\>testthat\</code\> (docs, CI/YAML, templates) may skip the
test-first step with a brief rationale.\</li\> \<li\>Keep changes
minimal and scoped to the requirement; don’t refactor or add unrelated
features in the same change.\</li\> \</ul\> \</div\> \<div
id="using-skills" class="section level3"\> \<h3\>Using Skills\</h3\>
\<ul\> \<li\>When a task matches an available skill’s
\<code\>description\</code\> (see \<a
href="skills/"\>\<code\>skills/\</code\>\</a\>), use that skill rather
than improvising — skills capture the ecosystem’s repeatable
workflows.\</li\> \<li\>Skills are guidance to adapt, not rigid scripts;
the Conventions here always apply on top of them.\</li\> \</ul\>
\</div\> \<div id="parallel-worktree-convention" class="section
level3"\> \<h3\>Parallel Worktree Convention\</h3\> \<p\>Multiple
development workflows may run concurrently in separate agent sessions.
To prevent agents from interfering with each other’s branches,
\<strong\>always use \<code\>git worktree\</code\>\</strong\> instead of
\<code\>git checkout\</code\> when starting work on a branch.\</p\>
\<div id="why" class="section level4"\> \<h4\>Why\</h4\>
\<p\>\<code\>git checkout\</code\> changes the branch in the shared
working directory. If two agents start work at the same time, one
agent’s checkout will silently switch the branch out from under the
other. Git worktrees give each branch its own isolated directory while
sharing a single \<code\>.git\</code\> object store.\</p\> \</div\>
\<div id="directory-layout" class="section level4"\> \<h4\>Directory
Layout\</h4\> \<pre\>\<code\>{repo}/ ← main worktree (stays on dev/main)
{repo}-worktrees/ {branch}/ ← linked worktree for the work
{other-branch}/ ← another concurrent branch\</code\>\</pre\> \<ul\>
\<li\>Place linked worktrees in \<code\>../{repo}-worktrees/\</code\> (a
sibling directory to the main repo clone).\</li\> \<li\>Name each
worktree directory after the branch.\</li\> \</ul\> \</div\> \<div
id="creating-a-worktree" class="section level4"\> \<h4\>Creating a
Worktree\</h4\> \<div class="sourceCode" id="cb4"\>\<pre
class="sourceCode bash"\>\<code class="sourceCode bash"\>\<span
id="cb4-1"\>\<a href="#cb4-1" tabindex="-1"\>\</a\>\<span class="co"\>#
From the main repo directory; base off the repo's integration branch
(dev if it exists, else main)\</span\>\</span\> \<span id="cb4-2"\>\<a
href="#cb4-2" tabindex="-1"\>\</a\>\<span class="fu"\>git\</span\> fetch
origin\</span\> \<span id="cb4-3"\>\<a href="#cb4-3"
tabindex="-1"\>\</a\>\<span class="fu"\>git\</span\> worktree add
../{repo}-worktrees/{branch} \<span class="at"\>-b\</span\> {branch}
origin/{base}\</span\>\</code\>\</pre\>\</div\> \</div\> \<div
id="working-in-a-worktree" class="section level4"\> \<h4\>Working in a
Worktree\</h4\> \<ul\> \<li\>\<strong\>All git and R/JS
commands\</strong\> for the work must run from within the worktree
directory.\</li\> \<li\>Use \<code\>cd\</code\> to enter the worktree at
the start of each phase. Do not assume the terminal is already
there.\</li\> \<li\>File edits (tests, source, docs) must target paths
inside the worktree, not the main clone.\</li\> \</ul\> \</div\> \<div
id="pushing-and-pr" class="section level4"\> \<h4\>Pushing and PR\</h4\>
\<ul\> \<li\>Push from the worktree directory: \<code\>git push -u
origin {branch}\</code\>.\</li\> \<li\>\<code\>gh pr create\</code\>
works normally from inside the worktree.\</li\> \</ul\> \</div\> \<div
id="cleanup" class="section level4"\> \<h4\>Cleanup\</h4\> \<p\>After
the PR is merged (or the work is abandoned):\</p\> \<div
class="sourceCode" id="cb5"\>\<pre class="sourceCode bash"\>\<code
class="sourceCode bash"\>\<span id="cb5-1"\>\<a href="#cb5-1"
tabindex="-1"\>\</a\>\<span class="co"\># From the main repo
directory\</span\>\</span\> \<span id="cb5-2"\>\<a href="#cb5-2"
tabindex="-1"\>\</a\>\<span class="fu"\>git\</span\> worktree remove
../{repo}-worktrees/{branch}\</span\> \<span id="cb5-3"\>\<a
href="#cb5-3" tabindex="-1"\>\</a\>\<span class="fu"\>git\</span\>
branch \<span class="at"\>-d\</span\>
{branch}\</span\>\</code\>\</pre\>\</div\> \<ul\> \<li\>Do not delete
worktrees for other agents’ in-flight work.\</li\> \<li\>If cleanup
fails because the worktree is still in use, skip it and inform the
user.\</li\> \</ul\> \</div\> \<div id="fallback" class="section
level4"\> \<h4\>Fallback\</h4\> \<p\>If the user explicitly requests
\<code\>git checkout\</code\> instead of worktrees (e.g., when only one
branch is active), the agent may fall back to the checkout flow. In all
other cases, default to worktrees.\</p\> \<hr /\> \</div\> \</div\>
\</div\> \<div id="skills" class="section level2"\> \<h2\>Skills\</h2\>
\<p\>Task-specific capabilities live as skills in \<a
href="skills/"\>\<code\>skills/\</code\>\</a\>, each a
\<code\>skills/\<name\>/SKILL.md\</code\> with a
\<code\>description\</code\> that tells the agent when to use it. The
full skill loads only when a task matches, so procedures don’t tax every
session. The agent invokes them as needed — they are flexible guidance,
not rigid scripts — and the Conventions above always apply on top.\</p\>
\<p\>Current skills:\</p\> \<ul\> \<li\>\<strong\>\<a
href="skills/tdd/SKILL.md"\>\<code\>tdd\</code\>\</a\>\</strong\> — the
Red-Green TDD development workflow (write failing tests, implement the
minimal change, then optionally open a PR), driven by a GitHub issue or
any stated requirement.\</li\> \<li\>\<strong\>\<a
href="skills/issue-review/SKILL.md"\>\<code\>issue-review\</code\>\</a\>\</strong\>
— preview a drafted GitHub issue, PR, or comment in the terminal and
capture sign-off before posting. Use when about to post a drafted
artifact (single or batch).\</li\> \<li\>\<strong\>\<a
href="skills/pkgdown-assets/SKILL.md"\>\<code\>pkgdown-assets\</code\>\</a\>\</strong\>
— create and publish examples, cookbooks, and slide decks on a pkgdown
site; guides through authoring source files,
\<code\>build_assets()\</code\>, local verification, and CI
wiring.\</li\> \<li\>\<strong\>\<a
href="skills/sub-issue-linking/SKILL.md"\>\<code\>sub-issue-linking\</code\>\</a\>\</strong\>
— link existing GitHub issues as sub-issues of a parent issue using the
Relationships feature. Use when adding sub-tasks or connecting
cross-repo issues to a parent requirement.\</li\> \</ul\> \<p\>Add a new
skill when a task type has a repeatable shape worth capturing; keep
one-off guidance in the relevant repo’s \<code\>AGENT.md\</code\>/docs
instead.\</p\> \</div\> \</div\>
