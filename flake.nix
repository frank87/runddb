{
	description = "A Rust web server including a NixOS module";

	# Nixpkgs / NixOS version to use.
	inputs.nixpkgs.url = "nixpkgs/nixos-23.05";

	outputs = { self, nixpkgs }:
	rec {
		system="x86_64-linux";
		packages.${system}.default = runddb;

		runddb =
			with import nixpkgs{ inherit system; };
			let
				deps = import ./rebar-deps.nix { inherit (pkgs) fetchHex fetchFromGitHub; };
			in
			pkgs.stdenv.mkDerivation{
				name = "runddb";
				src = self;

				buildPhase = ''
				mkdir -p _checkouts
				${toString (pkgs.lib.mapAttrsToList (k: v: ''
				cp -R --no-preserve=mode ${v} _checkouts/${k}
				'') deps)}
				HOME=. rebar3 tar
				'';

				installPhase = ''
				mkdir -p $out
				tarfile=_build/default/rel/*/*.tar.gz 
				tar -xzf $tarfile -C $out/

				set -x
				mkdir pack
				cd pack
				for dir in $( cd $out; find * -type d )
				do
					mkdir -p $dir
				done
				for file in $( cd $out; find * -type f )
				do
					ln -s $out/$file $file
				done
				rm bin/runddb
				cp -f $out/bin/runddb bin
				tar czvf $out/runddb-0.0.1.tar.gz *
				set +x
				echo "#!${pkgs.stdenv.shell}" > $out/depends.sh
				echo "${pkgs.gawk}/bin/awk" >> $out/depends.sh
				chmod a+x $out/depends.sh

				'';


				buildInputs = [ openssl rebar3 gnutar ];
			};

		# A NixOS module.
		nixosModule = { config, pkgs, ... }: {
				config = {
					system.nixos.tags = [ "runddb" ];
					
					users.users.runddb = { 
						isNormalUser = true; 
					};


					systemd.services."runddb" = {
						description = "Online REST database for cattle";

						wantedBy= [ "multi-user.target" ];

						path = with pkgs;[ gnutar runddb gzip gawk ];

						script = ''
								cd
								tar xzf ${runddb}/runddb-0.0.1.tar.gz
								bin/runddb  foreground
							'';

						serviceConfig = {
							Type="simple";
							User="runddb";

							WatchdogSec="10s";
							Restart="on-failure";
							runtimeDirectory = runddb;
						};

					};
				};
			};
	};
}
