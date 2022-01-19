# Generated by rebar3_nix
{ fetchHex, fetchFromGitHub }:
{
  ranch = fetchFromGitHub {
    owner = "ninenines";
    repo = "ranch";
    rev = "3190aef88aea04d6dce8545fe9b4574288903f44";
    sha256 = "1w04bypljnl4yvvnr58nzvavmfsk3h1h8mvwpz25w8c31jywgdc8";
  };
  enough = fetchHex {
    pkg = "enough";
    version = "0.1.0";
    sha256 = "sha256-BGDHq9pfXg6lkrErxpdrilxLluQvMyBZzTllJTdL+aE=";
  };
  cowlib = fetchFromGitHub {
    owner = "ninenines";
    repo = "cowlib";
    rev = "6deddc7d3360aa0f50eb2375cc0226157185c472";
    sha256 = "166vdi2s3nghq1zxcymjirqp9jlpjp73s37f2ydj7cgxnm4k3msn";
  };
  systemd = fetchHex {
    pkg = "systemd";
    version = "0.6.2";
    sha256 = "sha256-UGK5EYAMGrBRV8e/mp++I90kxYiRyH/RLS4+2PwXCLg=";
  };
  jsone = fetchFromGitHub {
    owner = "sile";
    repo = "jsone";
    rev = "f34edbc7fabf396a57c3f59a5827fb8fd7c4e59b";
    sha256 = "06aawr9rif6kiha4fvsi8wdq6vy945rsvpf1nhhqwfa540ybwv00";
  };
  iso8601 = fetchHex {
    pkg = "iso8601";
    version = "1.3.3";
    sha256 = "sha256-vMd2fWkeTYom5xP0jaUavZUb7E4HGuhB83F2b5a0aDQ=";
  };
  html_parser = fetchFromGitHub {
    owner = "hukl";
    repo = "html_parser";
    rev = "d3459ae3f1c2578570b5638553eeaaa377965124";
    sha256 = "09h306gh94npblp0p3pjgbqppxidxvjwiq1c89v2aj9m8n24cbgj";
  };
  erlydtl = fetchFromGitHub {
    owner = "erlydtl";
    repo = "erlydtl";
    rev = "cb6dc127585e4040465e38de0b7ac1d13ea83813";
    sha256 = "1aygv4c6yps4p5sjqwha8576v14ygf2j9rxnsz7x1vq896j853c8";
  };
  cowboy = fetchFromGitHub {
    owner = "ninenines";
    repo = "cowboy";
    rev = "8795233c57f1f472781a22ffbf186ce38cc5b049";
    sha256 = "0vwmy44h08a67m39ac6vd02mvvn3bcmavi4k8cdbrj0zvzddcilf";
  };
}
