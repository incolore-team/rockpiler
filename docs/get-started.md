# Get Started with Rockpiler

Rust is a systems programming language that runs blazingly fast, prevents segfaults, and guarantees thread safety. To get started with Rust, follow these steps:

1. Install Rust: <https://www.rust-lang.org/learn/get-started>
    To install Rust, run the following command in your terminal:

    ```shell
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```

    This will install the Rust toolchain, including the Rust compiler, Cargo package manager, and rustup, which is used to manage Rust versions.

2. Clone a Rust project:
    To clone a Rust project from a Git repository, navigate to your preferred directory and run the following command:

    ```shell
    git clone https://github.com/incolore-team/rockpiler
    cd rockpiler
    ```

    This will clone the "rockpiler" project from the incolore-team Github repository and navigate you to the project directory.

3. Build and test the project:
To build the project, run the following command in the project directory:

    ```shell
    cargo build
    ```

    This will build the project and compile any dependencies that are required.

    To run the test suite, run the following command:

    ```shell
    cargo test
    ```

    This will run all of the project's unit and integration tests and report the results.

    That's it! You are now ready to start working with Rust.
