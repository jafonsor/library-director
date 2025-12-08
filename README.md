# Library Director

A library management server built with the [Snap Framework](http://snapframework.com/) in Haskell, managed with Nix.

## Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled

To enable flakes, add the following to your `~/.config/nix/nix.conf`:

```
experimental-features = nix-command flakes
```

## Development

### Starting the Development Environment

Run the following command from the project root:

```bash
nix develop
```

This spawns a new shell with all development tools available:

- **GHC** — Glasgow Haskell Compiler
- **Cabal** — Build tool and package manager
- **HLS** — Haskell Language Server (IDE support)
- **HLint** — Linter for Haskell code
- **Ormolu** — Code formatter

All tools are pinned to specific versions via `flake.lock`, ensuring reproducible builds across machines. When you `exit` the shell, you return to your normal environment.

### Starting the Server

From within the `nix develop` shell:

```bash
cd backend
cabal run library-director
```

The server will start on `http://localhost:8000`.

### API Endpoints

| Method | Endpoint      | Description    |
| ------ | ------------- | -------------- |
| GET    | `/`           | Welcome page   |
| GET    | `/api/health` | Health check   |
| GET    | `/api/books`  | List all books |
| POST   | `/api/books`  | Add a new book |

## Dependency Management

### How `cabal.project.freeze` Works

The `library-director.cabal` file specifies dependencies **without version bounds**. This allows Cabal to resolve the latest compatible versions. The resolved versions are then locked in `cabal.project.freeze` for reproducible builds.

### Generating the Freeze File

If `cabal.project.freeze` doesn't exist or you want to regenerate it:

```bash
# Inside nix develop shell
cd backend

# Build to resolve dependencies
cabal build

# Lock the resolved versions
cabal freeze
```

This creates `backend/cabal.project.freeze` with exact version constraints:

```
constraints:
  aeson ==2.2.3.0,
  base ==4.19.0.0,
  snap-core ==1.0.5.1,
  ...
```

### Updating Dependencies

To update to newer package versions:

```bash
cd backend

# Remove the freeze file
rm cabal.project.freeze

# Update the package index
cabal update

# Rebuild (resolves latest versions)
cabal build

# Lock the new versions
cabal freeze
```

Commit the updated `cabal.project.freeze` to version control.

## Building with Nix

Build the project as a Nix derivation:

```bash
nix build
```

Run the built executable:

```bash
./result/bin/library-director
```

## Project Structure

```
library-director/
├── flake.nix                # Nix flake configuration
├── flake.lock               # Pinned Nix dependencies
├── README.md
└── backend/
    ├── library-director.cabal   # Package definition (no version bounds)
    ├── cabal.project            # Cabal project settings
    ├── cabal.project.freeze     # Locked dependency versions
    ├── src/                     # Library code
    │   ├── Application.hs       # Application state/snaplet
    │   └── Site.hs              # Route handlers
    └── src-main/                # Executable entry point
        └── Main.hs              # Server startup
```

## License

MIT
