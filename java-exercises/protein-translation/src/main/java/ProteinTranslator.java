import java.util.List;
import java.util.stream.*;

class ProteinTranslator {

    List<String> translate(String rnaSequence) {
        return Stream.iterate(0, i -> i + 3)
                .limit(rnaSequence.length() / 3)
                .map(i -> rnaSequence.substring(i, i + 3))
                .map(ProteinTranslator::proteinFor)
                .takeWhile(s -> !s.equals("STOP"))
                .collect(Collectors.toList());
    }

    static String proteinFor(String codon) {
        return switch (codon){
            case "AUG" -> "Methionine";
            case "UUU", "UUC" -> "Phenylalanine";
            case "UUA", "UUG" -> "Leucine";
            case "UCU", "UCC", "UCA", "UCG" -> "Serine";
            case "UAU", "UAC" -> "Tyrosine";
            case "UGU", "UGC" -> "Cysteine";
            case "UGG" -> "Tryptophan";
            case "UAA", "UAG", "UGA" -> "STOP";
            default -> throw new IllegalArgumentException("Invalid codon");
        };
    }
}
