import argparse
import torch
import pandas as pd
from minicons import scorer
# Add tqdm for pretty output in future?

def load_model(device, target_model, checkpoint):
    """Load target model using Minicons."""
    if target_model == 'gpt2':
        model_name = target_model
    elif target_model == 'gpt2m':
        model_name = 'gpt2-medium'
    elif target_model == 'gpt2l':
        model_name = 'gpt2-large'
    elif target_model == 'gpt2xl':
        model_name = 'gpt2-xl'
    elif target_model.split('_')[0] == '100M':
        model_name = f"models/{target_model}/checkpoint-{checkpoint}"
    else:
        print(f"Unrecognized model {target_model}")
    
    model = scorer.IncrementalLMScorer(model_name, device)

    return model


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Evaluate a model on test sentences.')
    parser.add_argument('testfile', default='data_analysis/stimuli/wider_pairs.csv')
    parser.add_argument('-m', '--model', type=str,
                        help="Location of model to test or size of public GPT-2 model (gpt2, gpt2m, gpt2l, gpt2xl)", default="s")
    parser.add_argument('-c', '--checkpoint', type=str,
                        help="Checkpoint number of model to test (unneeded for public models)", default="166800")
    args = parser.parse_args()
    target_model = args.model
    checkpoint = args.checkpoint

    if torch.cuda.is_available():
        device = "cuda"
    else:
        device = "cpu"
        print("CUDA not available!")

    model = load_model(device, target_model, checkpoint)

    test_sentences = pd.read_csv(args.testfile)
    do_sentences = test_sentences['DOsentence'].tolist()
    pd_sentences = test_sentences['PDsentence'].tolist()

    test_sentences['DO_score'] = model.sequence_score(do_sentences, reduction = lambda x: x.mean(0).item())
    test_sentences['PD_score'] = model.sequence_score(pd_sentences, reduction = lambda x: x.mean(0).item())

    test_sentences['diff'] = test_sentences['PD_score'] - test_sentences['DO_score']
    test_sentences.to_csv(f'data_analysis/data_raw/wider_pairs_scored_{target_model}_minicons.csv')
