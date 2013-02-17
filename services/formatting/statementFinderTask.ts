﻿//﻿
// Copyright (c) Microsoft Corporation.  All rights reserved.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

module Formatting {
    /// <summary>
    /// This task finds the closest statement's span before the given position
    /// </summary>
    export class StatementFinderTask {
        public BlockSpan: Span;

        constructor(private logger: TypeScript.ILogger, private semicolonPoint: SnapshotPoint, private fileAuthoringProxy: FileAuthoringProxy) {
            this.BlockSpan = null;
        }

        public Run(): void {
            var parseCursor = this.fileAuthoringProxy.GetASTCursor();
            //using (new ProxyReference(parseCursor))
            {
                parseCursor.SeekToOffset(this.semicolonPoint.position, /*excludeEndOffset*/ true);

                var nodes: AuthorParseNode[] = null;
                var foundSubTree = false;
                while (!foundSubTree) {
                    var children = parseCursor.GetSubTree(1);
                    //using (new ProxyReference(children))
                    {
                        if (children.Count() === 0)
                            return;

                        nodes = children.GetItems(0, children.Count());

                        if (nodes.length === 2 && nodes[1].Details.Kind === AuthorParseNodeKind.apnkList) {
                            parseCursor.MoveToChild(nodes[1].EdgeLabel, 0);
                        }
                        else {
                            foundSubTree = true;
                        }
                    }
                }

                for (var i = nodes.length - 1; i >= 0; i--) {
                    if (nodes[i].Details.EndOffset <= this.semicolonPoint.position) {
                        var startPos = nodes[i].Details.StartOffset;
                        this.BlockSpan = new Span(startPos, this.semicolonPoint.position - startPos + 1);   // +1 to include the semicolon itself in the span
                        break;
                    }
                }
            }
        }
    }
}
