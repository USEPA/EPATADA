import os
import requests

def parse_diff(diff_file):
    suggestions = []
    with open(diff_file, 'r') as f:
        lines = f.readlines()
        for line in lines:
            if line.startswith('@@'):
                parts = line.split(' ')
                line_info = parts[1].split(',')
                line_number = int(line_info[0].replace('-', ''))
            elif line.startswith('+'):
                suggestions.append((line_number, line[1:]))
                line_number += 1
            elif not line.startswith('-'):
                line_number += 1
    return suggestions

def create_review_comment(repo, pr_number, suggestions):
    url = f"https://api.github.com/repos/{repo}/pulls/{pr_number}/reviews"
    headers = {
        "Authorization": f"token {os.getenv('GITHUB_TOKEN')}",
        "Accept": "application/vnd.github.v3+json"
    }
    comments = [{"path": item[0], "line": item[1], "body": f"Suggestion: {item[2]}"} for item in suggestions]
    review_body = {
        "body": "Automated code style suggestions",
        "event": "COMMENT",
        "comments": comments
    }
    response = requests.post(url, json=review_body, headers=headers)
    if response.status_code != 201:
        raise Exception(f"Failed to create review comment: {response.content}")

def main():
    repo = os.getenv('GITHUB_REPOSITORY')
    pr_number = os.getenv('GITHUB_REF').split('/')[-1]
    diff_file = 'styled.diff'
    suggestions = parse_diff(diff_file)
    create_review_comment(repo, pr_number, suggestions)

if __name__ == "__main__":
    main()
