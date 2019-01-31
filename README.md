```bash
mkdir -p "${HOME}/.ssh"
ssh-keygen -t rsa -b 4096
pushd "${HOME}" && eval "$(curl -L https://raw.githubusercontent.com/micaelaneus/setup/master/init.sh)" && popd
```
