location ~ /(?:[^/]+)/InterventionEvaluatR/.session-data(/.*)$ {
	alias /mnt/session-data$1;
}
