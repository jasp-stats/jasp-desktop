FROM mcr.microsoft.com/devcontainers/cpp:ubuntu-24.04

ARG DEBIAN_FRONTEND=noninteractive
ARG QT_VERSION=6.10.3
ARG QT_MODULES="qtpositioning qtwebchannel qtwebengine qtwebsockets qtwebview debug_info qt5compat qtshadertools qtwaylandcompositor qthttpserver"

# 1) Base OS packages per Docs/development/jasp-build-guide-linux.md
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl git gnupg software-properties-common \
    build-essential cmake ninja-build \
    autoconf bison flex \
    g++ gfortran \
    zlib1g zlib1g-dev \
    libssl-dev \
    libgl1-mesa-dev \
    libsqlite3-dev \
    libarchive13 libarchive-dev \
    libxkbcommon-dev libxkbcommon-x11-dev libxcb-xkb-dev libxcb-xinerama0 libxcb-cursor0 \
    libglpk-dev \
    libminizip-dev \
    libfreexl-dev \
    libboost-dev libboost-filesystem-dev libboost-system-dev libboost-date-time-dev libboost-timer-dev libboost-chrono-dev \
    librdata-dev jags \
    libnss3-dev libnspr4-dev libxcomposite-dev libxdamage-dev libxrandr-dev libxtst-dev libxi-dev libasound2-dev libxkbfile-dev libxcb-icccm4-dev libxcb-shape0-dev libxcb-keysyms1-dev \
    libjsoncpp25 libjsoncpp-dev libxcb-xkb1 \
    r-base \
    libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libwebp-dev \
    libcairo2-dev libgsl-dev \
    fonts-noto-cjk fonts-arphic-ukai fonts-arphic-uming fonts-wqy-zenhei fonts-wqy-microhei \
    libsodium-dev \
    patchelf \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# 2) Install R from CRAN Ubuntu repo for a recent R (or use distro one if sufficient)
RUN set -eux; \
    . /etc/os-release; \
    echo "deb https://cloud.r-project.org/bin/linux/ubuntu ${UBUNTU_CODENAME}-cran40/" | tee /etc/apt/sources.list.d/cran-r.list; \
    curl -fsSL https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | gpg --dearmor -o /etc/apt/trusted.gpg.d/cran.gpg; \
    apt-get update && apt-get install -y --no-install-recommends r-base r-base-dev && rm -rf /var/lib/apt/lists/*

# 3) JAGS (use distro for convenience; can be replaced by source build later)
RUN apt-get update && apt-get install -y --no-install-recommends jags \
    && rm -rf /var/lib/apt/lists/*

# 4) Python + pip tooling for aqtinstall (Qt online installer alternative)
RUN apt-get update && apt-get install -y --no-install-recommends python3 python3-pip python3-venv && rm -rf /var/lib/apt/lists/*

# 5) Create venv for aqtinstall and install aqtinstall there
RUN python3 -m venv /opt/aqt-venv \
    && /opt/aqt-venv/bin/pip install --no-cache-dir aqtinstall

# 6) Install Qt with required modules via aqt from venv
RUN set -eux; \
    /opt/aqt-venv/bin/aqt install-qt -O /opt/Qt linux desktop ${QT_VERSION} linux_gcc_64 -m ${QT_MODULES}; \
    ln -s /opt/Qt/${QT_VERSION}/linux_gcc_64/bin/qmake /usr/local/bin/qmake || true

# 7) Install Qt Creator and other tools
RUN /opt/aqt-venv/bin/aqt install-tool -O /opt/Qt linux desktop tools_qtcreator qt.tools.qtcreator && \
    /opt/aqt-venv/bin/aqt install-tool -O /opt/Qt linux desktop tools_cmake && \
    /opt/aqt-venv/bin/aqt install-tool -O /opt/Qt linux desktop tools_ninja

# 8) Install readstat
RUN wget https://github.com/WizardMac/ReadStat/releases/download/v1.1.9/readstat-1.1.9.tar.gz && \
            tar -xzf readstat-*.tar.gz && cd readstat-*/ && \
            ./configure && make CFLAGS='-Wno-error=use-after-free' CXXFLAGS='-Wno-error=use-after-free' && make install
